
let explode s = 
    let rec explode_rec substr substr_len =
        match substr with
        | "" -> []
        | _ -> (String.get substr 0)::explode_rec (String.sub substr 1 (substr_len - 1)) (substr_len - 1)
    in
        explode_rec s (String.length s)
let rec implode chars =
    match chars with
    | [] -> ""
    | c::cs -> String.make 1 c ^ implode cs
