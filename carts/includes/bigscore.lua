-- big score helpers
-- credit: @Felice 
-- https://www.lexaloffle.com/bbs/?pid=22677
-- note: removed negative score support (??)
function score_tostr(v)
  local s=""
  repeat
      s=(v%0x0.000a/0x.0001)..s
      v/=10
  until v==0
  return s
end
