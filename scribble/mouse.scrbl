; for manual style
#lang scribble/manual

@(define myblog (link "https://alex-1q84.github.io" "blog"))

@table-of-contents{}

@title{On the Cookie-Eating Habits of Mice}

You can see the posts on my @|myblog|. @url{https://800best.com}

If you give a mouse a cookie, he's going to ask a glass of milk.

@section{The consequences of Milk}
That ``squeak'' was the mouse asking for milk. Let's
suppose that you give him some in a big glass.

He's a @smaller{small mouse}. the glass is too @larger{big}--@bold{way @larger{too
  @larger{big}}}. So, he'll @italic{probably} ask for you a straw. You might as
well give it to him.

@include-section["straw.scrbl"]

@section{Notice to Mice}

@centered{@bold{Notice to Mice}}

@margin-note{给小老鼠的一点要求。
 this is formated by margin-note}

this is an unordered item list
@itemlist[@item{We have cookies for you.}
          @item{If you want to eat a cookie,
           you must bring your own straw.}]

this is an ordered item list
@itemlist[#:style 'ordered
          @item{Eat cookie.}
          @item{Drink milk.}
          @item{Wipe mouth.}]

@tabular[#:sep @hspace[5]
         #:row-properties '(bottom-border ())
         (list (list @bold{Animal} @bold{Food})
               (list "mouse" "cookie")
               (list "moose" "muffin"))]

@centered{@bold{文本模式和 Racket 模式}}

粗略地说大括号包括的内容以文本模式对待，而方括号包括的内容则以 Racket 模式对待。
而一个操作并不在意它所使用的大括号还是方括号。

所以我们可以在文本中嵌入可执行的代码，例如

@verbatim|{
1 plus 1 is @(number->string (+ 1 1))
}| 渲染后结果

1 plus 1 is @(number->string (+ 1 1))

图片，任何能够转换成图片的变量都可以直接在 scribble 文档里使用，例如使用库 pict 和 2htdp/image 内的系列图片函数

自由地引用 racket 包
@(require pict)

This cookie has lost its chocolate chips:
@(colorize (filled-ellipse 40 40) "beige").

