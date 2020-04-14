
-- holes coordinates & metadata
local _bold_data={
	A={2,2,2,4},
	B={2,2},
	D={2,2,2,3},
	G={2,2,2,3},
	H={2,1,2,4},
	K={2,1,2,4},
	M={2,1,1,4,3,4},
	N={2,4},
	O={2,2,2,3},
	P={2,2},
	Q={2,2},
	R={2,2,2,4},
	U={2,1},
	V={2,1},
	W={1,1,3,1},
	X={2,1,2,4},
	Y={2,1},
	["0"]={2,1,2,2,2,3},
	["4"]={2,0},
	["6"]={2,3},
	["8"]={2,1,2,3},
	["9"]={2,1},
	["🅾️"]={glyph=true},
	["❎"]={glyph=true}
}
-- bold print
function printb(s,x,y,c)
	for k=1,#s do
		local t,bck,w=sub(s,k,k),{},6
		local holes,is_glyph=_bold_data[t]
		if holes then
			if(holes.glyph) w=7 is_glyph=true
			for i=1,#holes,2 do
				local sx,sy=x+holes[i]-1,y+holes[i+1]
				local pix=pget(sx,sy)
				-- backup background
				bck[i]=function() pset(sx,sy,pix) end
			end
		end

		if is_glyph then
      -- keep as is
      print(t,x,y,c)
		else
	  	-- bold print
      for i=-1,1 do
				print(t,x+i,y,c)
			end
		end

		-- recover char kerning
		for _,b in pairs(bck) do
			b()
		end
		x+=w
	end
end