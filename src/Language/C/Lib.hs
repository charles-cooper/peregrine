{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- mostly C++ wrapper stuff

module Language.C.Lib where
import Data.String.Interpolate
import Language.C.Utils

arrayTy :: Type -> Int -> C Type
arrayTy ty len = do
  include "array"
  return $ Type [i|std::array<${ty}, ${len}>|]

cReadIntegral :: Type -> C Func
cReadIntegral ty = do
  mapM include ["cstdint", "cassert", "cctype"]
  cfun impl
  return (Func funName)
  where
    funName = [i|parse_${ty}|]
    impl = [i|
      ${ty} ${funName} (char const *buf, ${uint} len) {
        ${ty} ret = 0;
        while (len--) {
          assert(isdigit(*buf));
          ret = ret * 10 + (*buf - '0');
          ++buf;
        }
        return ret;
      }
    |]

cpp_string :: C Type
cpp_string = do
  include "string"
  return $ Type [i|std::string|]

cpp_unordered_map :: Type -> Type -> C Type
cpp_unordered_map k v = do
  include "unordered_map"
  return $ Type [i|std::unordered_map<${k}, ${v}>|]

pair :: Type -> Type -> C Type
pair x y = do
  include "utility"
  return $ Type [i|std::pair<${x}, ${y}>|]

rolling_window :: Type -> Type -> (Exp -> Exp) -> (Exp -> Exp -> Exp) -> C Type
rolling_window dataTy accTy unOp binOp = do
  include "vector"
  cty window_class -- define the window class
  opName <- genId "op"
  cty (opTy opName)
  return $ Type [i|window<${dataTy}, ${accTy}, struct ${opName}>|]
  where
    opTy opName = [i|
      struct ${opName} {
        ${accTy} operator ()(${accTy} const &x, ${accTy} const &y) const {
          return ${binOp (Exp "x") (Exp "y")};
        }
        ${accTy} operator ()(${dataTy} const &x) const {
          return ${unOp (Exp "x")};
        }
      };
      |]
    window_class = [i|
      template<typename T, typename Acc, typename Op>
      class window
      {
#if DEBUG_CONTAINER
        public :
#endif
        Op op;
        typedef struct
        {
          T   data;
          Acc acc;
        } element_t;
        std::vector<element_t> front;
        std::vector<element_t> back;

        public :
        void push(T const &t) {

          if (front.empty()) {
            // Push the first element onto the front stack.
            // Initialize its accumulator to the value.
            element_t e;
            e.data = t;
            e.acc = op(t);
            front.push_back(e);

          } else {
            // Push an element onto the front stack.
            // accumulating whatever is already there.
            element_t e;
            e.data = t;
            e.acc = op(op(t), front.back().acc);
            front.push_back(e);

          }
        }

        void pop(void) {

          if (empty()) {
            return;
          }

          if (back.empty()) {

            // push the first element onto the back stack.
            element_t e = front.back();
            e.acc = op(e.data);
            back.push_back(e);
            front.pop_back();

            // now for every element, pop it off the front, and push
            // it onto the back, running the accumulator.
            // actually don't pop every element off the front since
            // that would be slow. just loop and then clear the front
            for (size_t i = front.size(); i != 0; i--) {
              element_t e = front[i - 1];
              e.acc = op(op(e.data), back.back().acc);
              back.push_back(e);
            }

            front.clear();

            // back is now guaranteed to have at least one element
            back.pop_back();

          } else {
            back.pop_back();
            return;
          }
        }

        // Peek oldest element in the queue
        T const &peek_back(void) const {
          // UNDEFINED BEHAVIOR
          if (empty()) {
            return front.front().data;
          }

          // The oldest element in the FIFO queue
          // is the BOTTOM of the first LIFO stack
          // if the second LIFO stack is empty,
          // otherwise it is the TOP of the second
          // LIFO stack.

          // The oldest element in the FIFO queue
          // is the BOTTOM of the first LIFO stack.
          if (back.empty()) {
            return front.front().data;
          }

          // The oldest element in the FIFO queue
          // is the top of the second LIFO stack
          return back.back().data;

        }

        // Peek newest element in the queue
        T const &peek_front(void) const {
          if (empty()) {
            return front.front().data;
          }
          // The newest element in the FIFO queue
          // is the TOP of the first LIFO stack.
          // If the first LIFO stack is empty,
          // it is the BOTTOM of the second LIFO stack.
          if (front.empty()) {
            return back.front().data;
          }

          return front.back().data;
        }

        Acc accumulate(void) const {
          // UNDEFINED BEHAVIOR
          if (empty()) {
            return front.front().acc;
          }

          if (back.empty()) {
            return front.back().acc;
          }

          if (front.empty()) {
            return back.back().acc;
          }

          return op(front.back().acc, back.back().acc);
        }

        size_t size(void) const {
          return front.size() + back.size();
        }

        bool empty(void) const {
          return front.empty() && back.empty();
          // is `return !size();` faster since it avoids a branch?
        }
      };
      |]

