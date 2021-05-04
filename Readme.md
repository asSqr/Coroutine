### これは何？
JS の Generator で do 構文っぽいことができるという話 (https://github.com/pelotom/burrido) について調べている．もともと uhyo さんのレポジトリ (https://github.com/uhyo/infinite-recursion.macro) を見て思い出した．

### 論文とか資料
## 読んだ

- Stackless Scala With Free Monads
- Coroutine Pipelines - Mario Blažević, The Monad.Reader issue 19, pages 29-50

### まだ読んでない

- https://hackage.haskell.org/package/monad-coroutine-0.9.1/docs/Control-Monad-Coroutine.html
- Trampolined Style - Ganz, S. E. Friedman, D. P. Wand, M, ACM SIGPLAN NOTICES, 1999, VOL 34; NUMBER 9, pages 18-27
- The Essence of Multitasking - William L. Harrison, Proceedings of the 11th International Conference on Algebraic
Methodology and Software Technology, volume 4019 of Lecture Notes in Computer Science, 2006
- https://pursuit.purescript.org/packages/purescript-free/4.0.1/docs/Control.Monad.Trampoline
- https://qiita.com/41semicolon/items/985bdd2f551d9392463c / https://qiita.com/uhyo/items/21e2dc2b9b139473d859 
- https://blog.uhy.ooo/entry/2020-05-23/babel-macro/
- https://gist.github.com/gakuzzzz/2737698 
- https://gist.github.com/MgaMPKAy/7976436 
- https://xuwei-k.hatenablog.com/entry/20150209/1423443779 -
- https://qiita.com/masaki_shoji/items/cdb03ad870e1fc7cb175 -
- https://en.wikipedia.org/wiki/Continuation-passing_style 
- http://blog.functionalfun.net/2008/04/bouncing-on-your-tail.html 
- https://qiita.com/yumura_s/items/3f3ae0e87f46b344bdce
- https://stackoverflow.com/questions/57733363/how-to-adapt-trampolines-to-continuation-passing-style
- http://dspace.library.uvic.ca/bitstream/handle/1828/5879/Saba_Sahand_MSc_2015.pdf?sequence=1&isAllowed=y 
- https://en.wikipedia.org/wiki/Coroutine#Comparison_with_mutual_recursion 
- http://www.berniepope.id.au/assets/files/haskell-mpi.monad.reader.pdf#page=29 
- https://myuon.github.io/posts/coroutine-monad-as-state-machine/
- https://dl.acm.org/doi/pdf/10.1145/3110249 
- https://drops.dagstuhl.de/opus/volltexte/2018/9208/pdf/LIPIcs-ECOOP-2018-3.pdf

### 雑記
Generator は Coroutine というものの機能を制限したものということらしい．

値のログを残す yield (Generator) と値を取りだす await (Iteratee) は双対．それぞれ，Writer モナドと Reader モナド．では JS の yield は State か？

外側の世界と内側の世界で関係が裏返り双対？ Corecursion は何か関係ある？