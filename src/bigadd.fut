def carry_prop (c1 : u64) (c2 : u64) =
    (c1 & c2 & 2) | (( (c1 & (c2 >> 1)) | c2 ) & 1)
    -- 

def badd0 [m] (ipb : i64) (n : i64) 
          (ash : [m]u64) (bsh : [m]u64)
          : [m]u64 =
    ???

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
    in (badd0 ipb n ash bsh) :> [ipb*(4*n)]u64