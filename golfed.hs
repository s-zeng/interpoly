import Data.List
main=getLine>>=(putStrLn.f.p.(map(read::String->Double)).words)
e=zip[0..]
i(x:y)=all((<0.1).abs.((-)x))y
d 0 l=l
d n l=d(n-1)$tail$zipWith(-)l$0:l
t=True
g l
 |i l=0
 |t=(+1).g$d 1 l
s l
 |g l==0=[a]
 |t=[a]++s[x-a*(n^(g l))|(n,x)<-e l] where a=(d(g l)l)!!0/(product[1..g l])
p l=s l++replicate((g l)+1-length(s l))0
b=reverse
f=(intercalate" + ").(y<$>).b.e.b.(show<$>) where y(a,b)=b++"x^"++show a
