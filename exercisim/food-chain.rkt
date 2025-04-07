#lang racket

(provide recite)

(define animals
  ;;(No. (name desc spec))
  '((1 . ("fly" "I don't know why she swallowed the fly. Perhaps she'll die." "."))
    (2 . ("spider" "It wriggled and jiggled and tickled inside her." " that wriggled and jiggled and tickled inside her."))
    (3 . ("bird" "How absurd to swallow a bird!" "."))
    (4 . ("cat" "Imagine that, to swallow a cat!" "."))
    (5 . ("dog" "What a hog, to swallow a dog!" "."))
    (6 . ("goat" "Just opened her throat and swallowed a goat!" "."))
    (7 . ("cow" "I don't know how she swallowed a cow!" "."))
    (8 . ("horse" "She's dead, of course!" "."))))

(define (get-animal n)
  (car (cdr (assoc n animals))))

(define (get-animal-desc n)
  (cadr (cdr (assoc n animals))))

(define (get-animal-spec n)
  (caddr (cdr (assoc n animals))))

(define (build-verse pos)
  (cond
    [(< pos 8) (flatten
                (append (list (format "I know an old lady who swallowed a ~a." (get-animal pos))
                              (if (= pos 1) null (get-animal-desc pos)))
                        (for/list ([n (in-range pos 1 -1)])
                          (format "She swallowed the ~a to catch the ~a~a"
                                  (get-animal n) (get-animal (sub1 n)) (get-animal-spec (sub1 n))))
                        (list (get-animal-desc 1))))]
    [else (list (format "I know an old lady who swallowed a ~a." (get-animal pos))
                (get-animal-desc pos))]))

(define (insert-every-other lst item)
  (cond
    [(empty? lst) lst]
    [(empty? (cdr lst)) lst]
    [else (cons (car lst)
                (cons item
                      (insert-every-other (cdr lst) item)))]))

(define (recite start-verse end-verse)
  (cond
    [(and (= start-verse end-verse) (<= end-verse 8))
     (build-verse end-verse)]
    [else (flatten (insert-every-other
                    (for/list ([n (in-range start-verse (add1 end-verse))])
                      (build-verse n))
                    ""))]))


(module+ test
  (require rackunit rackunit/text-ui)
  (define suite
    (test-suite "food chain tests"
                (test-equal? "fly"
                             (recite 1 1)
                             '("I know an old lady who swallowed a fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."))
                (test-equal? "spider"
                             (recite 2 2)
                             '("I know an old lady who swallowed a spider."
                               "It wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."))
                (test-equal? "bird"
                             (recite 3 3)
                             '("I know an old lady who swallowed a bird."
                               "How absurd to swallow a bird!"
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."))
                (test-equal? "cat"
                             (recite 4 4)
                             '("I know an old lady who swallowed a cat."
                               "Imagine that, to swallow a cat!"
                               "She swallowed the cat to catch the bird."
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."))
                (test-equal? "dog"
                             (recite 5 5)
                             '("I know an old lady who swallowed a dog."
                               "What a hog, to swallow a dog!"
                               "She swallowed the dog to catch the cat."
                               "She swallowed the cat to catch the bird."
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."))
                (test-equal? "goat"
                             (recite 6 6)
                             '("I know an old lady who swallowed a goat."
                               "Just opened her throat and swallowed a goat!"
                               "She swallowed the goat to catch the dog."
                               "She swallowed the dog to catch the cat."
                               "She swallowed the cat to catch the bird."
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."))
                (test-equal? "cow"
                             (recite 7 7)
                             '("I know an old lady who swallowed a cow."
                               "I don't know how she swallowed a cow!"
                               "She swallowed the cow to catch the goat."
                               "She swallowed the goat to catch the dog."
                               "She swallowed the dog to catch the cat."
                               "She swallowed the cat to catch the bird."
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."))
                (test-equal? "horse"
                             (recite 8 8)
                             '("I know an old lady who swallowed a horse."
                               "She's dead, of course!"))
                (test-equal? "multiple verses"
                             (recite 1 3)
                             '("I know an old lady who swallowed a fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."
                               ""
                               "I know an old lady who swallowed a spider."
                               "It wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."
                               ""
                               "I know an old lady who swallowed a bird."
                               "How absurd to swallow a bird!"
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."))
                (test-equal? "full song"
                             (recite 1 8)
                             '("I know an old lady who swallowed a fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."
                               ""
                               "I know an old lady who swallowed a spider."
                               "It wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."
                               ""
                               "I know an old lady who swallowed a bird."
                               "How absurd to swallow a bird!"
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."
                               ""
                               "I know an old lady who swallowed a cat."
                               "Imagine that, to swallow a cat!"
                               "She swallowed the cat to catch the bird."
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."
                               ""
                               "I know an old lady who swallowed a dog."
                               "What a hog, to swallow a dog!"
                               "She swallowed the dog to catch the cat."
                               "She swallowed the cat to catch the bird."
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."
                               ""
                               "I know an old lady who swallowed a goat."
                               "Just opened her throat and swallowed a goat!"
                               "She swallowed the goat to catch the dog."
                               "She swallowed the dog to catch the cat."
                               "She swallowed the cat to catch the bird."
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."
                               ""
                               "I know an old lady who swallowed a cow."
                               "I don't know how she swallowed a cow!"
                               "She swallowed the cow to catch the goat."
                               "She swallowed the goat to catch the dog."
                               "She swallowed the dog to catch the cat."
                               "She swallowed the cat to catch the bird."
                               "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
                               "She swallowed the spider to catch the fly."
                               "I don't know why she swallowed the fly. Perhaps she'll die."
                               ""
                               "I know an old lady who swallowed a horse."
                               "She's dead, of course!"))))
  (run-tests suite))
