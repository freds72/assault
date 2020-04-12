pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
local fixit={
 a={2,1,2,4},
 b={2,1,2,3},
 d={2,1,2,2,2,3},
 g={2,1,2,2,2,3},
 h={2,0,2,4},
 k={2,0,2,4},
 m={2,0,1,4,3,4},
 n={2,4},
 o={2,1,2,2,2,3},
 p={2,1},
 q={2,1,2,2},
 r={2,1,2,4},
 u={2,0},
 v={2,0},
 w={1,0,3,0,2,4},
 x={2,0,2,4},
 y={2,0},
 [0]={2,1,2,2,2,3},
 [4]={2,0},
 [6]={2,3},
 [8]={2,1,2,3},
 [9]={2,1}
}
function printb(s,x,y,c)
 for k=1,#s do
	 local t,bck=sub(s,k,k),{}
		local fix=fixit[t]
		if fix then
			for i=1,#fix,2 do
			 local sx,sy=x+fix[i]-1,y+fix[i+1]+1
			 local pix=pget(sx,sy)
				bck[i]=function() pset(sx,sy,pix) end
			end
		end
 
 	for i=-1,1 do
	  print(t,x+i,y,c)
 	end
 	
 	for _,b in pairs(bck) do
 	 b()
 	end
 	--print(t,x,y,0)
 	x+=6
 end
end

function _draw()
 cls()
 
 printb("SCORE",65,55,1)
 printb("SCORE",64,54,14)

 printb(tostr(flr(time()*64)),65,65,1)
 printb(tostr(flr(time()*64)),64,64,7)

 print(stat(1),2,2,7)
end
__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000000000000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
