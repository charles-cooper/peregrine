# FAQ

### What is Peregrine?

Peregrine is an embedded domain specific language (EDSL) for implementing automated trading strategies. At its core, it abstracts the usual callback-based architecture of an automated trading system by using signals. By leveraging the composability of signals, a trading system can decouple its alpha generation, risk management and order management constituents.

Peregrine is the core of the Peregrine Framework, which is a framework using the Peregrine language which includes feed and order handlers, research and optimization, and a runtime system for running strategies.

### What is a callback and why do people use them?

Traditionally, people use callbacks because they map directly onto how trading systems communicate with each other - with messages. Every time the system receives a message, it initiates a callback to notify any subsystems that the message has been received (which in turn may initiate callbacks to *their* subsystems).

### What is a signal and what are its benefits over callbacks?

A signal is either a raw message or a combination of signals. A limiting factor of callback-based systems is their *composability*. Given a callback-based system that calculates total-dollar-value-traded and one that calculates total-shares-traded it is not easy to combine them into a system that calculates vwap, even though vwap is just the former divided by the latter. Signals bring us back to this intuition by representing vwap simply as total-dollar-value-traded / total-shares-traded.

### I'm concerned about performance. All these abstractions sound nice in theory but in practice abstraction often results in bloated data structures and code.

Peregrine was designed from the ground up not only to make writing performant programs easy, but even writing non-performant programs difficult. Peregrine is compiled into C++, and it is easy to predict the type of code it will generate. In terms of data structures, it makes use of `stl`'s `unordered_map`, `vector` and that's it. It does not use allocation besides the bulk allocations `stl` does internally. All the core operations are O(1), and care was taken to make data structures and execution paths local. As a demonstration please see [https://github.com/charles-cooper/itch-order-book](https://github.com/charles-cooper/itch-order-book) which is written using some of the above techniques to build an order book which achieves 62ns / tick throughput including message parsing and syscalls.

### What about extensibility?

Peregrine can produce libraries which are called by or call into C code.

### What is composability?

Composability is the ability to easily piece together systems out of smaller pieces, and then piece together larger systems out of those pieces without knowing how big or complex the internal structure of those subsystems are. That means that the following are all signals:

```
TAQ.quote.bidPrice
TAQ.quote.askPrice
TAQ.quote.midpoint = (TAQ.quote.bidPrice + TAQ.quote.askPrice) / 2
TAQ.quote.weightedMidpoint =
    (TAQ.quote.bidPrice * TAQ.quote.bidSize
      + TAQ.quote.askPrice * TAQ.quote.askSize)
  / (TAQ.quote.askSize + TAQ.quote.bidSize)
```

Given our definition of `weightedMidpoint` we can just as easily use it as an input to our system as if it were provided to us via the underlying protocol.

Compare that to a possible implementation in a callback-based system where calculation and dependency tracking are inextricably intertwined:

```
calculateWeightedMidpoint() {
  this->_weightedMidpoint =
      (this->_bidPrice * this->_bidSize
        + this->_askPrice * this->_askSize)
    / (this->_bidSize + this->_askSize);
}
onBid(bidPrice, bidSize) {
  this->_bidPrice = bidPrice; this->_bidSize = bidSize;
  this->_weightedMidpoint = calculateWeightedMidpoint();
  ...
}
onAsk(askPrice, askSize) {
  this->_askPrice = askPrice; this->_askSize = askSize;
  this->_weightedMidpoint = calculateWeightedMidpoint();
  ...
}
```

Even in this small example several weaknesses of the callback implementation are revealed:
1. The calculation of `weightedMidpoint` needs to be factored into a separate function to prevent code duplication.
2. The programmer has to remember to call `calculateWeightedMidpoint()` in every location where one of the quantities it depends on can change.
3. Worst of all, the weighted midpoint calculation can't be easily reused. In order to propagate it to other parts of the system, one either has to manually write in calls to further callbacks after every call to `calculateWeightedMidpoint` (indicated by the `...` sections), OR write in the calls to further callbacks in `calculateWeightedMidpoint`. The former results in code duplication and manual dependency tracking, the latter breaks the reusability of calculateWeightedMidpoint outside of its place in this particular system's set of callbacks.

Peregrine solves these problems by automatically calculating the dependencies of any signal and making sure they are calculated at the correct time in the stack of callbacks.

### How can this help me "decouple" alpha generation, risk management and order management?

In a callback-based system, the programmer has to carefully intertwine all calculations that depend on each other (see above examples). In Peregrine you no longer need to be so careful since the system automates dependency tracking for you. That means that each component can just depend on the *final* values calculated by the other two components. Consider the following toy strategy (for the sake of keeping the size of the example down, exit logic is omitted):

*(Note that since Peregrine is still under development, this is a 'pseudocode' version of Peregrine)*
```
recentSharesAtAsk = do
  sharesAtAsk <- guard (tradePrice == askPrice) tradeSize
  sum (rolling_window (10 * seconds) sharesAtAsk)
recentSharesAtBid = do
  sharesAtBid <- guard (tradePrice == bidPrice) tradeSize
  sum (rolling_window (10 * seconds) sharesAtBid)
buySignal = recentSharesAtAsk / recentSharesAtBid

riskCheck = do
  sumGroupBy orderId (orderQty * orderPrice) < maxOpenDollarValue

strategy = do
  groupBy symbol $ do
    guard (openQty == 0) $ do
      guard (buySignal > 10.0) $ do
        guard riskCheck $ do
          submitOrder askPrice defaultQty
```

Despite the decidedly simple nature of the strategy, it already has several properties:
1. Everything was defined once. We didn't need to define classes with the values we wanted to keep track of, and then make sure we threaded the values appropriately. We just defined what the signals were in terms of each other and that was it.
2. Separation of concerns. The alpha signal doesn't need to know about the order manager, *even its existence*, in order for the latter to define a valid strategy. Nor does it need to know about the global risk manager. This makes it easy to mix and match signals with different risk management or order management styles. For instance, if it is later discovered that our alpha is more potent in mid-cap stocks and less potent in small- and large-caps, we can easily add on an adjustment to the risk management part which increases relative size deployed in mid-caps, which can be applied to one, some, or across the board to all strategies. Or, we can add an additional sharpe-based risk management in addition to the total-quantity-based risk management.

### Why is Peregrine an embedded domain-specific language (EDSL) instead of a standalone language?

We made the decision to embed Peregrine in Haskell, a modern functional language known for its productivity and correctness guarantees. The benefits of embedding in Haskell, which is a pure functional language with a mature ecosystem include:
1. Peregrine programs can be written as functions of each other. This makes it even easier to generate Peregrine programs and compose them together.
2. Advanced users can easily implement their own protocols and extensions.
3. Since Peregrine is a domain specific language, we can perform static analysis of Peregrine programs specific to its domain. One feature we are working on is using advanced verification techniques to prove properties about Peregrine programs, for instance that a Peregrine program will never have more than a certain number of outstanding orders.
4. Ease of embedding in a larger production system. For instance, a framework we think will be of interest to users is the ability to define a signal as a function of some free parameters, optimize it over historical data overnight and run the next day, all in a few lines of code.
