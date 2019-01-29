(TeX-add-style-hook
 "proof-tree"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("standalone" "convert={density=300,size=300x300,outext=.png}")))
   (TeX-run-style-hooks
    "latex2e"
    "standalone"
    "standalone10"
    "tikz"))
 :latex)

