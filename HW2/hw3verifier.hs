main = do
	let s1 = buildS2 Cats Hug Bunnies
	let s2 = buildS1 Cats Cuddle
	let s3 = buildNP Silly Ducks
	let s4 = buildNAnd Dogs Cats
	let s5 = conjunction [s2, s1]
	let s6 = buildS2 s3 Chase s4	
	let s7 = buildS1 Dogs Scare
	let s8 = conjunction [s1, s2, s7]
	let s9 = buildNP Old s3
	let s10 = buildS2 s9 Cuddle Cats

	putStrLn " s1 "
	print (s1)
	print (pretty(s1))
	putStrLn" Is nice s1 "
	print (isNice s1)
	putStrLn" Word count s1 "
	print (wordCount s1)

	putStrLn " s2 "
	print (s2)
	print (pretty(s2))
	putStrLn" Is nice s2 "
	print (isNice s2)
	putStrLn" Word count s2 "
	print (wordCount s2)

	putStrLn " s5 "
	print (s5)
	print (pretty(s5))
	putStrLn" Is nice s5 "
	print (isNice s5)
	putStrLn" Word count s5 "
	print (wordCount s5)  

	putStrLn " s6 "
	print (s6)
	print (pretty(s6))
	putStrLn" Is nice s6 "
	print (isNice s6)
	putStrLn" Word count s6 "
	print (wordCount s6)
	
	putStrLn " s7 "
	print (s7)
	print (pretty(s7))
	putStrLn" Is nice s7 "
	print (isNice s7)
	putStrLn" Word count s7 "
	print (wordCount s7)
	
	putStrLn " s8 "
	print (s8)
	print (pretty(s8))
	putStrLn" Is nice s8 "
	print (isNice s8)
	putStrLn" Word count s8 "
	print (wordCount s8)
	
	putStrLn " s10 "
	print (s10)
	print (pretty(s10))
	putStrLn" Is nice s10 "
	print (isNice s10)
	putStrLn" Word count s10 "
	print (wordCount s10)