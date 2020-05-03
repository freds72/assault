-- helper functions for bold punny font & number 
-- holes coordinates & metadata

local _bold_data=json_parse'{"A":[2,2,2,4],"B":[2,2],"D":[2,2,2,3],"G":[2,2,2,3],"H":[2,1,2,4],"K":[2,1,2,4],"M":[2,1,1,4,3,4],"N":[2,4],"O":[2,2,2,3],"P":[2,2],"Q":[2,2],"R":[2,2,2,4],"U":[2,1],"V":[2,1],"W":[1,1,3,1],"X":[2,1,2,4],"Y":[2,1],"0":[2,1,2,2,2,3],"4":[2,0],"6":[2,3],"8":[2,1,2,3],"9":[2,1]}'

-- bold print
function printb(s,x,y,c)
	do_printb(s,x+1,y+1,true)
	do_printb(s,x,y,false,c)
end
-- print an array of messages
function printba(msgs)
	for _,msg in pairs(msgs) do
		printb(msg.txt,msg.x,msg.y,msg.c)
	end
end
function do_printb(s,x,y,shadow,c)
	local _bold_data=_bold_data
	for k=1,#s do
		local ch,bck=sub(s,k,k),{}
		local o,w=ord(ch),6
		-- numbers+glyphs: white on blue
		-- text: pink on black
		if c then
			color(c)
		elseif o>47 and o<58 then
			color(shadow and 1 or 7)
		elseif o>128 then
			w=7
			color(shadow and 1 or 7)
		else
			color(shadow and 0 or 14)
		end
		local holes=_bold_data[ch]
		if holes then
			for i=1,#holes,2 do
				local sx,sy=x+holes[i]-1,y+holes[i+1]
				local pix=pget(sx,sy)
				-- backup background
				bck[i]=function() pset(sx,sy,pix) end
			end
		end

		-- glyph?
		if o>128 then
      -- keep as is
      print(ch,x,y)
		else
	  	-- bold print
      for i=-1,1 do
				print(ch,x+i,y)
			end
		end

		-- recover char kerning
		for _,b in pairs(bck) do
			b()
		end
		x+=w
	end
end
