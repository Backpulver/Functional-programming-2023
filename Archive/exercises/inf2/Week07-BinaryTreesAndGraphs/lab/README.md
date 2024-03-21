# Двоични дървета и графи

```racket
(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))
```

### Задача 1
Реализирайте процедури, които връщат елементите на двоично дърво в списък:
 - `inorder`
 - `preorder`
 - `postorder`

```racket
(equal? (inorder our-cool-tree) '(2 1 4 3 5))
(equal? (preorder our-cool-tree) '(1 2 3 4 5))
(equal? (postorder our-cool-tree) '(2 4 5 3 1))
```

### Задача 2
Реализирайте процедурa, която намира броя на елементите в двоично дърво.

```racket
(= (size our-cool-tree) 5)
```

### Задача 3
Реализирайте процедурa, която намира височината на двоично дърво.

```racket
(= (height our-cool-tree) 3)
```

### Задача 4
Реализирайте процедура, която проверява дали едно двоично дърво е балансирано. Едно двоично дърво е балансирано, ако:
 - е празно
 - лявото му и дясното му поддърво са балансирани и разликата между
височините на лявото и дясното поддърво е 0 или 1

```racket
(equal? (balanced? our-cool-tree) #t)
```

### Задача 5
Реализирайте процедура, която проверява дали едно двоично дърво е перфектно балансирано. Едно двоично дърво е перфектно балансирано, ако:
 - е празно
 - лявото му и дясното му поддърво са перфектно балансирани и разликата между
броя елементи на лявото и дясното поддърво е 0 или 1

```racket
(equal? (perfectly-balanced? our-cool-tree) #f)
```

### Задача 6
Реализирайте процедури `min` и `max`, които приемат компаратор и двоично дърво и намират съответно най-малкия и най-големия елемент на дървото спрямо този компаратор.

```racket
(= (min < our-cool-tree) 1)
(= (max < our-cool-tree) 5)
```

### Задача 7
Реализирайте процедура, която връща всички листа на двоично дърво.

```racket
(equal? (leaves our-cool-tree) '(2 4 5))
```

### Задача 8
Реализирайте процедура, която връща елементите на ниво `n` в двоично дърво.

```racket
(equal? (at-level 0 our-cool-tree) '(1))
(equal? (at-level 1 our-cool-tree) '(2 3))
(equal? (at-level 2 our-cool-tree) '(4 5))
```

### Задача 9
Реализирайте процедура, която приема унарна функция `f` и я прилага върху всеки елемен на двоично дърво.

```racket
(equal? (map add1 our-cool-tree) '(2 (3 () ())
                                    (4 (5 () ())
                                        (6 () ()))))
```

### Задача 10
Реализирайте процедура, която приема унарен предикат `pred?` и връща списък с всички елементи на двоично дърво, които удовлетворяват условието.

```racket
(equal? (filter even? our-cool-tree) '(2 4))
```

```racket
(define our-cool-graph '((1 . (2 3))
                         (2 . (3))
                         (3 . (4 5))
                         (4 . ())
                         (5 . (2 4 6))
                         (6 . (2))))
```

### Задача 11
Реализирайте процедура, която връща списък от точкови двойки от видя `'(u . v)`, показващи, че има ребро от `u` до `v`.

```racket
(equal? (edges our-cool-graph) '((1 . 2) (1 . 3) (2 . 3) (3 . 4) (3 . 5) (5 . 2) (5 . 4) (5 . 6) (6 . 2)))
```

### Задача 12
Реализирайте процедура, която приема унарен предикат `pred?` и връх `v` и връща списък от децата на `v`, които удовлетворяват този предикат.

```racket
(equal? (filter-children our-cool-graph even? 3) '(4))
```

### Задача 13
Реализирайте процедура, която премахва връх `v` от граф.

```racket
(equal? (remove-vertex 5 our-cool-graph) '((1 2 3)
                                          (2 3)
                                          (3 4)
                                          (4)
                                          (6 2)))
```

### Задача 14
Реализирайте процедура, която проверява дали има път от `u`до `v` в граф.

```racket
(equal? (path? 1 6 our-cool-graph) #t)
(equal? (path? 5 1 our-cool-graph) #f)
```

### Задача 15
Реализирайте процедури, които обхождат граф в дълбочина и в широчина. Напишете подходящи тестове.