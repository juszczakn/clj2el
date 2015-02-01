# clj2el
Introducing some clojure-esque constructs to elisp

# Usage
Currently, support is very low and hacky. clj2el provides three main ways of evaluating buffers.

```clj2el-eval-buffer``` runs reader-macros and then ```eval-buffer```.

```clj2el-compile-buffer-dont-eval``` runs reader-macros, saves a copy of the buffer.

```clj2el-compile-buffer-and-eval``` runs reader-macros, saves a copy of the buffer and then evals it.

## Installation
Download clj2el and add to your emacs load-path. I suggest putting it in ~/.emacs.d/manual-packages

Clone clj2el:

    mkdir -p ~/.emacs.d/manual-packages
    cd ~/.emacs.d/manual-packages && git clone https://github.com/juszczakn/clj2el.git

Add to load-path in .emacs or init.el:

    (add-to-list 'load-path "~/.emacs.d/manual-packages/clj2el")
    (require 'clj2el)        ; reader and namespace-clashing functions
    (require 'clj2el-core)   ; core functions
    
## Examples
Here's some simple examples:

    ; maps
    {:a 1 :b {:a 1 :b 2}} ;=> (clj2el-make-hash-table :a 1 :b (clj2el-make-hash-table :a 1 :b 2))
    ; labmdas
    #(+ % %1)             ;=> (lambda (&optional % %2 %3 %4) (+ % %2))
    
    (defn f [x]
      (+ 1 x))
      
    (let [x "Hello"
          y "world!"]
        (str x " " y)    ;=> "Hello world!"
