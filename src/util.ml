let counter = ref 0

let unique_id s = 
    let id = String.concat "" [s; (BatString.of_int (!counter))] in
    incr counter;
    id