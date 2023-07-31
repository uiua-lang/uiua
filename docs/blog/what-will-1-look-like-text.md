# What will Uiua 1.0 look like?

2024-01-19

The [Uiua pad](https://uiua.org/pad) page prominently displays the words "Uiua is not yet stable". And so it has been asked: when will Uiua be stable? What features will it have? Is there a roadmap?

This post is to organize and present my thoughts on the future of Uiua.

## Stability

Uiua will be made officially stable only after it has been unofficially stable for some time. That is, not until no breaking changes have been made for a long time.

The following language features will need to be nailed down before Uiua can ever be stable.

### Stack manipulation

I think working with the stack, at least for up to 3 values, has become mostly pretty nice. However, things start to get complicated when working with more values, as is often necessary. There is some design work to be done here, and it's not out of the question that a very small amount of non-tacitness could be introduced to improve this.

The experimental [`bind`](https://uiua.org/docs/bind) modifier is a potential solution to this problem.

There is a balance to be struc between Uiua's goal of tacitness and its goal of being ergonomic. While the beauty of fully tacit code is a worthy goal, some problems involve data flows that are inherently complex, and so some kind of labeling system may be necessary to make such problems workable.

### Box Ergonomics

While I've explored alternatives, I've come to the conclusion that nested arrays are a necessary pest. The data we work with is often nested or ragged, and while there are ways to represent such data with flat structures, those representations are cumbersome in their own ways.

And so boxes are likely here to stay. However, I do think some design work can be done to improve their ergonomics. Currently, Uiua's boxes are very similar to J's, but I think it may be worth it to make their usage a bit more implicit in some cases, closer to the nested arrays of APL or BQN.

### System APIs

The current [system functions](https://uiua.org/docs/system) are useful and *mostly* work. There are definitely implementation gaps which need to be filled. There are a good number of missing filesystem operations, and some other things like UDP sockets and proper interaction with child processes still need to be implemented.

### FFI

An FFI system similar to [BQN's](https://mlochbaum.github.io/BQN/spec/system.html#foreign-function-interface) is planned. This will allow Uiua to call into C libraries and will enable a lot more functionality.