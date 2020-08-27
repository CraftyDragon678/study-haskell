하스켈과 같은 함수형 프로그래밍 언어에서는 원래
콘솔 출력과 같은 `side-effect`를 만들 수 없다.

그렇지만 모나드를 이용하면 부가 정보(=side-effect)
와 연산 부분(purely functional)을 분리할 수 있다.

putStrLn이 그 예.
IO Monad라고 부른다.
