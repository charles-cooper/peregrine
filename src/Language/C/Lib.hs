{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.C.Lib where
import Data.String.Interpolate
import Language.C.Utils

cReadIntegral ty = do
  mapM include ["cstdint", "cassert", "cctype"]
  cfun impl
  return funName
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

rolling_window :: Type -> (Exp -> Exp -> Exp) -> C Type
rolling_window ty op = do
  include "vector"
  cty window_class
  opName <- genId "op"
  cty (opTy opName)
  return $ Type [i|window<${ty}, struct ${opName}>|]
  where
    opTy opName = [i|
      struct ${opName} {
        ${ty} operator ()(${ty} x, ${ty} y) {
          return ${op (Exp "x") (Exp "y")};
        }
      };
      |]
    window_class = [i|
      template<typename T, typename Op>
      class window
      {
        Op op;
        typedef struct
        {
          T data;
          T acc;
        } element_t;
        std::vector<element_t> front;
        std::vector<element_t> back;

        void push(T const &t) {

          if (front.empty()) {
            // Push the first element onto the front stack.
            // Initialize its accumulator to the value.
            element_t e;
            e.data = t;
            e.acc = t;
            front.push_back(e);

          } else {
            // Push an element onto the front stack.
            // accumulating whatever is already there.
            element_t e;
            e.data = t;
            e.acc = op(t, front.back().acc);
            front.push_back(e);

          }
        }

        void pop(void) {

          if empty() {
            return;
          }

          if (back.empty()) {

            // push the first element onto the back stack.
            element_t e = front.back();
            e.acc = e.data;
            back.push_back(e);
            front.pop_back();

            // now for every element, pop it off the front, and push
            // it onto the back, running the accumulator.
            // actually don't pop every element off the front since
            // that would be slow. just loop and then clear the front
            for (size_t i = front.size(); i != 0; i--) {
              element_t e = front[i - 1];
              e.acc = op(e.data, back.back().acc);
              back.push_back(e);
            }

            front.clear();

            // back is now guaranteed to have at least one element
            back.pop();

          } else {
            back.pop();
            return;
          }
        }

        T accumulate(void) const {
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

        void empty(void) const {
          return front.empty() && back.empty();
          // is `return !size();` faster since it avoids a branch?
        }
      };
      |]
