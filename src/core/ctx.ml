type t = { row : Row.t; node : string }

let make ~row ~node = { row; node }
let row t = t.row
let id t = t.row.id
let attempt t = t.row.attempt
let max_attempts t = t.row.max_attempts
let is_final_attempt t = t.row.attempt >= t.row.max_attempts
let queue t = t.row.queue
let worker t = t.row.worker
let meta t = t.row.meta
let node t = t.node
let errors t = t.row.errors
