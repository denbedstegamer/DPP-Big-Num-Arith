def carry_prop (c1 : u32) (c2 : u32) =
    (c1 & c2 & 2) | (( (c1 & (c2 >> 1)) | c2 ) & 1)
    -- 1 del - anden bit bestemmer cascading overflows (mx1 && mx2)
    -- 2 del - første bit bestemmer reelle overflows   ((ov1 && mx2) || ov2)

def badd0 [m] (ash : [m]u64) (bsh : [m]u64)
          : [m]u64 =
    let (res, c) = map2 (\a b -> 
                            let s = a + b
                            let c = u32.bool (s < a)
                            let c = c | ((u32.bool (s == u64.highest)) << 1)
                            in (s, c)
                        ) ash bsh |> unzip
    -- In principle, c and carry_prop could use u2, u4, or u8 instead
    -- Assuming registers always have at least 32 bits, this doesn't change much
    let carries = scan carry_prop 2u32 c
    in map2 (\r i -> r + u64.bool (i > 0 && (carries[i-1] & 1 == 1))) res (0..<m)
    

def bigadd [ipb] [n] (as : [ipb*(4*n)]u64) 
                     (bs : [ipb*(4*n)]u64) 
                     : [ipb*(4*n)]u64 =
    let g = ipb * n
    let cp2sh (i : i32) =
        let g = i32.i64 g in
        ((as[i], as[g + i], as[2 * g + i], as[3 * g + i]), 
         (bs[i], bs[g + i], bs[2 * g + i], bs[3 * g + i]))
    let (ass, bss) = (0..<g) |> map i32.i64 |> map cp2sh |> unzip
    let (a1s, a2s, a3s, a4s) = unzip4 ass
    let (b1s, b2s, b3s, b4s) = unzip4 bss
    let ash = a1s ++ a2s ++ a3s ++ a4s
    let bsh = b1s ++ b2s ++ b3s ++ b4s
    -- ovenstående flytter as -> ash, b -> bsh, hvor de nye adresser er i shared memory

    in (badd0 ash bsh) :> [ipb*(4*n)]u64