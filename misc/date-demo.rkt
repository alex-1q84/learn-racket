#lang racket
(require racket/date)

(define my-date (seconds->date 1753252603.17244))

(date-display-format 'iso-8601)
(date->string my-date 1753252603.17244)


#|
方法 seconds->date 生成的日期存储在 struct date* 中，其结构定义为
(struct date* date (nanosecond time-zone-name)) ，例如下面 my-date
变量展现出的值为 (date* 43 36 14 23 7 2025 3 203 #f 28800 172440052 "CST")
各字段所代表意思分别为
(date* second
minute
hour
day
month
year
week-day       ;day-of-week-start-from-one
year-day       ;day-of-year
dst?           ;表明是否含夏令时调整，如果有夏令时调整则调整秒数也会含在 time-zone-offset 中
time-zone-offset ;相对 UTC 时区的偏移秒数
nanosecond
timezone)

有了上面这个 struct 我们可以很容易实现日期格式化或日期和秒之间的转换、时区处理等
|#
my-date
(current-date)