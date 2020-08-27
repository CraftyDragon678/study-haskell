-- parser: 텍스트 조각을 분석해, syntaxtic structure를 만들어 내는 프로그램
-- GHC는 haskell을 파싱하고, 브라우저는 html을 파싱

-- 함수형 언어에서 파서는 함수?
type Parser = String -> Tree
-- 오류가 날 수도 있으니
type Parser = String -> (Tree, String)
-- 여러가지로 해석될 수도 있으니
type Parser = String -> [(Tree, String)]
-- 파서가 꼭 트리를 만들 필요는 없음
type Parser a = String -> [(a, String)]
-- 여기 쓰인 a가 약간 그 제너릭 고런거임,,,
