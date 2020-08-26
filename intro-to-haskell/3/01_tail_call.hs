factorial 0 = 1
factorial n = n * factorial(n - 1)
-- 꼬리 재귀 최적화 때문에 스택이 쌓이지 않음
