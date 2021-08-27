<?xml version="1.0" encoding="utf-8"?>
<!-- Created by Leo: http://leoeditor.com/leo_toc.html -->
<leo_file xmlns:leo="http://leoeditor.com/namespaces/leo-python-editor/1.1" >
<leo_header file_format="2"/>
<globals/>
<preferences/>
<find_panel_settings/>
<vnodes>
<v t="mywo.20210826203644.2"><vh>learnning racket</vh></v>
<v t="mywo.20210826203816.1"><vh>beautiful racket</vh>
<v t="mywo.20210826203843.1"><vh>@path beautiful_racket</vh>
<v t="mywo.20210826204040.1"><vh>@path stackerizer</vh>
<v t="mywo.20210826204138.1"><vh>@clean stackerizer.rkt</vh></v>
<v t="mywo.20210826204508.1"><vh>@clean stackerizer-test.rkt</vh></v>
</v>
</v>
</v>
</vnodes>
<tnodes>
<t tx="mywo.20210826203644.2"></t>
<t tx="mywo.20210826203816.1">https://beautifulracket.com</t>
<t tx="mywo.20210826203843.1"></t>
<t tx="mywo.20210826204040.1"></t>
<t tx="mywo.20210826204138.1">@language racket
#lang br/quicklang
(provide + *)
(provide (rename-out [stackerizer-mb #%module-begin]))

(define-macro (stackerizer-mb EXPR)
  #'(#%module-begin
     EXPR))

(define-macro-cases +
  [(+ FIRST) #'FIRST]
  [(+ FIRST NEXT ...) #'(list 'dyadd FIRST (+ NEXT ...))])
</t>
<t tx="mywo.20210826204508.1">@language racket
#lang s-exp "stackerizer.rkt"

(+ 1 2 3 4)
</t>
</tnodes>
</leo_file>
