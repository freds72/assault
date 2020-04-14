-- big score helpers
-- credit: @Felice 
-- https://www.lexaloffle.com/bbs/?pid=22677
function score_add(score,n)
  return score+shr(n,16) 
end
-- note: removed negative score support (??)
function score_tostr(v)
  local s=""
  repeat
      s=(v%0x0.000a/0x.0001)..s
      v/=10
  until v==0
  return s
end
