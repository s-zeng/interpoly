import Data.List
main :: IO ()
main=getLine>>=(putStrLn.f.p.(map(read::String->Double)).words)
e=zip[0..]
i(x:y)=all((< 0.1).abs.(subtract x))y
d 0 l=l
d n l=d(n-1)$tail$zipWith(-)l$0:l
t=True
g l
 |i l=0
 |t=succ.g$d 1 l
s l
 |g l==0=[a]
 |t=[a]++s[x-a*(n^(g l))|(n,x)<-e l] where a=(head$d(g l)l)/fromIntegral(product[1..g l])
p l=s l++replicate((g l)+1-length(s l))0
b=reverse
f=(intercalate" + ").(map y).b.e.b.(map show) where y x=snd x++"x^"++show(fst x)
