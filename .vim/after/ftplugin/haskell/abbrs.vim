iab <buffer> ++ <>
iab <buffer> >> *>
iab <buffer> mapM traverse
iab <buffer> sequence sequenceA
iab <buffer> forM_ for_
iab <buffer> `ap` <*>
iab <buffer> `fmap` <$>
iab <buffer> `liftA` <$>
iab <buffer> `liftM` <$>
iab <buffer> (<$>) fmap
iab <buffer> (<*>) liftA
iab <buffer> `mappend` <>
"iab <buffer> `sconcat` <>
iab <buffer> concat join
iab <buffer> deriving deriving (Show, Eq)
iab <buffer> filter mfilter
iab <buffer> liftM liftA
iab <buffer> liftM2 liftA2
iab <buffer> liftM3 liftA3
iab <buffer> main main :: IO ()
iab <buffer> map fmap
iab <buffer> return pure
iab <buffer> != /=
iab <buffer> $! $
iab <buffer> {-# {-# LANGUAGE #-}

let s:vars = ['a', 'b', 'c', 's', 'f', 'm', 'y', 'x'] 
let s:vars = s:vars + map(s:vars, "v:val + 's'") + map(range(0, 9), 'string(v:val)')

"let s:ops = [
            "\ '->', 
            "\ '=>', 
            "\ '>=', 
            "\ '<.>', 
            "\ '<=<', 
            "\ '>=>', 
            "\ '!!', 
            "\ '\|\|', 
            "\ '&&', 
            "\ '&', 
            "\ '<=', 
            "\ '=', 
            "\ '>', 
            "\ '<', 
            "\ '/', 
            "\ '//', 
            "\ '*', 
            "\ '+', 
            "\ '-', 
            "\ '`', 
            "\ '::', 
            "\ ':', 
            "\ '<>', 
            "\ '>>', 
            "\ '<<', 
            "\ '<\|>', 
            "\ '<$>', 
            "\ '<*>',
            "\ '<**>',
            "\ ]

let s:ops = []

for s:var in s:vars
    for s:op in s:ops
        exe 'iab <buffer> '.s:var.s:op.' '.s:var.' '.s:op
        exe 'iab <buffer> '.s:op.s:var.' '.s:op.' '.s:var
        for s:var2 in s:vars
            exe 'iab <buffer> '.s:var.s:op.s:var2.' '.s:var.' '.s:op.' '.s:var2
        endfor
    endfor
endfor
