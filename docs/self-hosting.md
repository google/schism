Schism is a self hosted compiler. Each revision of Schism is built by a
previous revision of Schism. It's sometimes important to understand how this
works in order to add new functionality to Schism, as it affects when you are
allowed to use certain features. Schism has four stages: stage0, stage1,
stage2, and stage3.

Stage0 is also called the snapshot. We keep it checked into the repo, but it
doesn't have to be (and probably shouldn't be). It's a working version of the
compiler in binary form (Wasm, in this case), that is used to jump start the
rest of the compilation process. Note that you can also use
bootstrap-from-guile.sh to regenerate this.

Stage1 is Schism compiled by the stage0 compiler. I'll say more about this in a
second.

Stage2 is Schism compiled by the stage1 compiler. From time to time, we "update
the snapshot," which basically means we rename schism-stage2.wasm to
schism-stage0.wasm, so the stage 2 compiler becomes the new snapshot.

There's also a stage3 compiler, which is Schism compiled by the stage2
compiler. This should be exactly the same as the stage2 compiler, so we don't
generally build the stage3 compiler. From time to time we do though to make
sure stage 2 and 3 actually are identical.

The distinction between the stages comes up when adding optimizations or
features to the compiler.

If you add a new optimization, you would write it into Schism, at compiler.ss.
You then compile this into the stage1 compiler using the stage0 compiler. The
stage1 compiler now knows how to do your optimization, but it has not itself
been optimized. Stage2 would then both be able to perform the optimization, and
has also had the optimization applied. If you look at compiler performance in
this case, it's pretty common for stage1 to be slower than stage0, because it
has to do additional work to perform the optimization, But then, stage2 is
often faster than both stage0 and stage1 because it has been optimized by the
new optimization.

When adding a new feature, such as when I was adding support for lambdas, you
have to implement the feature without using it in the compiler. Once you've
implemented it, stage1 can compile code with the new feature. So, if you have
test cases that use the feature, these will start to pass. However, you can't
use the new feature in the compiler until you take a new snapshot, so that
stage0 will support the feature as well.
