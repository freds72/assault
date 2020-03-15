pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
-- rotomap
function rsprtexmap(m,px,py,pz,a)
	local ca,sa=cos(a)*pz,sin(a)*pz
	local ddx0,ddy0=ca,sa
	local ddx1,ddy1=ca,sa
	local ddx2,ddy2=2*ca,2*sa
	local ddx3,ddy3=3*ca,3*sa
	local ddx4,ddy4=4*ca,4*sa
	local ddx5,ddy5=5*ca,5*sa
	local ddx6,ddy6=6*ca,6*sa
	local ddx7,ddy7=7*ca,7*sa
	ca,sa=cos(a),sin(a)
	local w=shl(pz,7)
	px,py=px-w,py-w
	local dx0,dy0=(sa-ca)*(w/2-0.5)+w,w-(ca+sa)*(w/2-0.5)

	-- dx/dy: offset in full texmap space (1024x1024)
	local mem,dx,dy=0x6000,8*ddx0,8*ddy0
	-- mdx: offset in texmap space (256x1024)
	local mdx1,mdx2,mdx3,mdx4,mdx5,mdx6,mdx7=shr(ddx0,3),shr(2*ddx0,3),shr(3*ddx0,3),shr(4*ddx0,3),shr(5*ddx0,3),shr(6*ddx0,3),shr(7*ddx0,3)
	for iy=0,127 do
		local srcx,srcy,mx=dy0+px,dx0+py,shr(dy0+px,3)
		for ix=0,15 do

			-- find pixel 8-pixel row
			local s0,s1,s2,s3,s4,s5,s6,s7=
				m[bor(band(mx,0xff),band(shl(srcy,8),0xff00))],
				m[bor(band(mx+mdx1,0xff),band(shl(srcy-ddy1,8),0xff00))],
				m[bor(band(mx+mdx2,0xff),band(shl(srcy-ddy2,8),0xff00))],
				m[bor(band(mx+mdx3,0xff),band(shl(srcy-ddy3,8),0xff00))],
				m[bor(band(mx+mdx4,0xff),band(shl(srcy-ddy4,8),0xff00))],
				m[bor(band(mx+mdx5,0xff),band(shl(srcy-ddy5,8),0xff00))],
				m[bor(band(mx+mdx6,0xff),band(shl(srcy-ddy6,8),0xff00))],
				m[bor(band(mx+mdx7,0xff),band(shl(srcy-ddy7,8),0xff00))]
			-- get x%8 pixel
			-- shift back to least sig byte (int)
			poke4(
				mem,
				bor(
					bor(
						bor(rotr(band(shl(s0,shl(band(srcx,7),2)),0xf000),28),
						rotr(band(shl(s1,shl(band(srcx+ddx1,7),2)),0xf000),24)),
						bor(rotr(band(shl(s2,shl(band(srcx+ddx2,7),2)),0xf000),20),
						rotr(band(shl(s3,shl(band(srcx+ddx3,7),2)),0xf000),16))
					),
					bor(
					bor(rotr(band(shl(s4,shl(band(srcx+ddx4,7),2)),0xf000),12),
					rotr(band(shl(s5,shl(band(srcx+ddx5,7),2)),0xf000),8)),
					bor(rotr(band(shl(s6,shl(band(srcx+ddx6,7),2)),0xf000),4),
					band(shl(s7,shl(band(srcx+ddx7,7),2)),0xf000))
					)
				)
			)  		
			mem+=4
			srcx+=dx
			srcy-=dy
			mx+=ddx0
		end
		dy0+=ddy0
		dx0+=ddx0
	end
end

-- restore background
function restoremap(backup,dst)			
	for k,v in pairs(backup) do
		dst[k]=v
	end
end

-->8 
-- return a rotated version of the sprite in a table
function rspr(sx,sy,w,a,tc)
	tc=tc or 0
	local ca,sa=cos(a),sin(a)
	local ddx0,ddy0=ca,sa
	local mask=shl(0xfff8,w-1)
	w*=4
	local dx0,dy0=w+(sa-ca)*(w-0.5),w-(ca+sa)*(w-0.5)
	w*=2
	local s={}
	for ix=0,w-1 do
		local srcx,srcy=dx0,dy0
		for iy=0,w-1 do
			local c=tc
			if band(bor(srcx,srcy),mask)==0 then
				c=sget(sx+srcx,sy+srcy)
			end
			-- store 1 pixel per cell
			s[ix+iy*w]=c

			srcx-=ddy0
			srcy+=ddx0
		end
		dx0+=ddx0
		dy0+=ddy0
	end
	return s
end

-- in-memory sprite
-- includes support for rotated sprites
function make_memspr(sx,sy,w,h,nangles,tc)
	nangles=nangles or 1
	-- transparent color
	tc=tc or 0

	local angles={}
	for i=0,nangles-1 do
		local a=i/nangles-0.25
		local spr=rspr(sx,sy,flr(w/8),a,tc)

		-- convert sprite to 8 pixel dword's
		local s,smask,dw={},{},0
		for y=0,h-1 do
			local mask,b,bmask=0x1000
			for x=0,w-1 do
				local c=spr[x+y*w]
				b=bor(b or 0,c*mask)
				bmask=bor(bmask or 0,(c==tc and 0xf or 0)*mask)
				-- shift to next row
				mask=lshr(mask,4)
				if mask==0 then
					mask=0x1000
					s[dw],smask[dw],b,bmask=b,bmask
					dw+=1
				end
			end
			if b then
				s[dw],smask[dw]=b,bmask
				dw+=1
			end
			-- dword padding
			s[dw],smask[dw]=0,0xffff.ffff
			dw+=1
		end	

		angles[i]={s=s,smask=smask}
	end		

	-- default sprite
	local s,smask=angles[0].s,angles[0].smask

	-- width in dword unit+padding
	w=flr(w/8)+1

	return {
		rotate=function(self,angle)
			-- select sprite cache entry
			local cache=angles[flr(nangles*((angle%1+1)%1))]
			s,smask=cache.s,cache.smask
		end,
		draw=function(self,dst,x,y)

			-- dword boundary
			local ix=flr(x/8)
			-- localize globals
			local w,s,smask,shift,dx=w,s,smask,shl(flr(x)-8*ix,2),ix+256*flr(y)
			local backup,bmask={},lshr(0xffff.ffff,shift)
			for j=0,h-1 do
				local srcx,dstx,v0mask,v0=j*w,dx+shl(j,8),0xffff.ffff
				for i=0,w-1 do
					-- stride (shifted)
					local v1,v1mask=rotr(s[srcx],shift),rotr(smask[srcx],shift)						
					local c,v,vmask=
						-- back color
						dst[dstx],
						-- current stride + previous
						bor(
							band(v1,bmask),
							band(v0,bnot(bmask))),
						-- stride transparency mask + previous
						bor(
							band(v1mask,bmask),
							band(v0mask,bnot(bmask)))
					-- backup background color
					backup[dstx]=c
					-- merge with back color										
					dst[dstx]=bor(
						band(c,vmask),
						band(v,bnot(vmask)))
									
					v0,v0mask=v1,v1mask
					srcx+=1
					dstx+=1
				end
			end
			return backup
		end
	}
end

-->8
local shkx,shky=0,0
function cam_shake()
	shkx,shkx=min(1,shkx+rnd()),min(1,shky+rnd())
end
function cam_update()
	shkx*=-0.7-rnd(0.2)
	shky*=-0.7-rnd(0.2)
	if abs(shkx)<0.5 and abs(shky)<0.5 then
		shkx,shky=0,0
	end
end

function lerp(a,b,t)
	return a*(1-t)+t*b
end


-- game
local time_t=0
local plyr={
 	x=28,y=22,z=2,
 	dz=-0.01,
	acc=0,
	angle=0,
	da=0,
	w=0.4,
	h=0.4,
	sx=48,
	sy=0
}

-- in memory map of sprite coords
local _texmap={}

-- check for the given tile flag
function fmget(x,y)
	return fget(mget(x,y))
end
-- return true if solid tile
function solid(x,y)
	return fget(mget(x,y),0)
end

function get_area(a,dx,dy)
	local x,y,w,h=a.x+dx,a.y+dy,a.w,a.h
	return 
		bor(
			bor(fmget(x-w,y-h),
			fmget(x+w,y-h)),
			bor(fmget(x-w,y+h),
			fmget(x+w,y+h)))
end

local bullet=make_memspr(16,32,8,8)

local parts={}
function update_parts(m)
	for i,p in pairs(parts) do
		local x0,y0=p.x,p.y
		local x1,y1=x0+p.dx,y0+p.dy
		-- bullet
		if p.kind==0 then
			if solid(x0,y1) or solid(x1,y0) then
				parts[i]=nil
			else
				p.x=x1
				p.y=y1
			end
		-- nuke
		elseif p.kind==1 then
			p.x+=p.dx
			p.y+=p.dy
			p.z+=p.dz
			if p.z<=1 then
				make_nuke(p.x,p.y)
				parts[i]=nil
			end
			-- gravity
			p.dz-=0.004
		elseif p.update then
			parts[i]=p:update()
		end
	end
end
function draw_parts(m)
	local ca,sa=cos(plyr.angle),sin(plyr.angle)
	local x0,y0,z0=plyr.x,plyr.y,plyr.z
	for _,p in pairs(parts) do
		local x,y,w=(p.x-x0)/z0,(p.y-y0)/z0,p.w/z0
		local dx,dy=64+shl(ca*x-sa*y,3)-w/2,64+shl(sa*x+ca*y,3)-w/2
		if p.draw then
			p:draw(dx,dy)
		else
			-- todo: select rotated sprite
			sspr(p.sx,p.sy,p.w,p.w,dx,dy,w,w)
		end
	end
end

function make_part(sx,sy,kind,x,y,z,dx,dy,dz)
	return add(parts,{
		sx=sx,
		sy=sy,
		kind=kind,
		x=x,
		y=y,
		z=z,
		w=8,
		dx=dx or 0,
		dy=dy or 0,
		dz=dz or 0
	})	
end

local nukes={}
local dither_pat={0b1111111111111111,0b0111111111111111,0b0111111111011111,0b0101111111011111,0b0101111101011111,0b0101101101011111,0b0101101101011110,0b0101101001011110,0b0101101001011010,0b0001101001011010,0b0001101001001010,0b0000101001001010,0b0000101000001010,0b0000001000001010,0b0000001000001000,0b0000000000000000}
local fadetable={
	{5,13},
	{13,6},
	{13,6},
	{13,6},
	{14,15},
	{13,6},
	{6,7},
	{7,7},
	{14,14},
	{10,15},
	{10,15},
	{11,6},
	{12,6},
	{6,6},
	{14,15},
	{15,7}
   }
   
function fade(i)
	for c=0,15 do
		if flr(i+1)>=3 then
			pal(c,7,1)
		else
			pal(c,fadetable[c+1][flr(i+1)],1)
		end
	end
end

function make_nuke(x,y,z)
	local r0,r1,fp,t=8,8,#dither_pat,0
	local nuke=make_part(0,0,2,x,y,z)
	nuke.update=function(self)
		t+=1
		if(t>33) return
		return self
	end
	nuke.draw=function(self,x,y)
		--
		if t<4 then
			fade(t)
		else
			pal()
			r0=lerp(r0,45,0.22)/plyr.z
			r1=lerp(r1,45,0.3)/plyr.z
			local rr0,rr1=r0*r0,r1*r1
			if(t>22) fp=lerp(fp,1,0.1)	
			fillp(dither_pat[flr(fp)]+0x0.ff)
			camera(-x,-y)
			color(0x77)
			for r=0,r1 do
				local x0,x1=max(rr0-r*r)^0.5,(rr1-r*r)^0.5
				rectfill(-x0,r,-x1,r)
				rectfill(x0,r,x1,r)
				rectfill(-x0,-r,-x1,-r)
				rectfill(x0,-r,x1,-r)    
			end
			circ(0,0,r0,12)
			fillp()
			camera()
		end
	end
end

local jumppads={}

function _init()
	-- x: 8 pixel blocks
	-- y: 1 pixel line
	for j=0,255 do
		for i=0,40 do
			-- sprite tile
			local s=mget(i,j/8)
			-- jumpad?
			if fget(s,2) then
				jumppads[i+256*flr(j/8)]=3
			end
			local sx,sy,b=band(shl(s,3),127),shl(flr(s/16),3)+j%8,0
			for shift=0,7 do
				b=bor(b,sget(sx+shift,sy)*lshr(0x1000.0000,4*shift))
			end
			_texmap[i+shl(j,8)]=b
		end
	end

	-- test particules
end

function _update()
	time_t+=1
	cam_update()

	if(btn(0)) plyr.da=-0.01
	if(btn(1)) plyr.da=0.01
	
	plyr.angle+=plyr.da
	plyr.da*=0.8
	
	local dx,dy=cos(-plyr.angle-0.75),sin(-plyr.angle-0.75)
	if btnp(5) then
		-- airborne?
		if plyr.z>1 then
			make_part(16,48,1,plyr.x,plyr.y,plyr.z,dx/2,dy/2)
		else
			make_part(16,56,0,plyr.x,plyr.y,plyr.z,dx/2,dy/2)
		end
	end

	-- apply thrust force
	local dx,dy=cos(plyr.angle),sin(plyr.angle)
	if(btn(2)) plyr.acc+=0.02
	if(btn(3)) plyr.acc-=0.02
	plyr.acc*=0.92

	dx,dy=-dy*plyr.acc,-dx*plyr.acc

	-- get tile
	local xarea,yarea=get_area(plyr,dx,0),get_area(plyr,0,dy,0)
	-- solid?
	if band(xarea,0x1)==0 then
		plyr.x+=dx
	end
	if band(yarea,0x1)==0 then
		plyr.y+=dy
	end
	-- gravel?
	local area=bor(xarea,yarea)
	if band(area,0x2)>0 and abs(plyr.acc)>0.1 then
		cam_shake()
	-- jumppad?
	elseif band(area,0x4)>0 then
		local i=bor(flr(plyr.x),shl(flr(plyr.y),8))
		local j=jumppads[i]
		-- actvivate
		if j and j>0 and plyr.z==1 then
			plyr.dz=0.05
			-- 
			jumppads[i]-=1
		end
	end

 	plyr.z=max(1,plyr.z+plyr.dz)
 	plyr.dz+=0.5*plyr.dz*plyr.dz-0.002

	update_parts(_texmap)
end

local red_blink={0,1,2,2,8,8,8,2,2,1}

function _draw()

	-- rotating map
	rsprtexmap(_texmap,8*plyr.x+shkx,8*plyr.y+shky,plyr.z,plyr.angle)	

	-- player
	spr(96,56,56,2,2)

	draw_parts(_texmap)

	local ca,sa=16*cos(plyr.angle),16*sin(plyr.angle)
	line(64,64,64+ca,64+sa,11)
	line(64,64,64-sa,64+ca,8)

	--rot_plane(-plyr.angle)
	pal(8,red_blink[flr((5.3*time())%#red_blink)+1],1) 

	rectfill(0,0,127,8,1)
	print(stat(1).." - "..stat(0),2,2,7)
	print(#parts,2,8,7)
end

__gfx__
0000000000000000000110001111d7d111000000dddddddddddddddddddddddd000000000000000000000000000000001111499a4112442442442114a9941111
0000000000000000001511dd11551d1155115100dd77d77d77d77d77d77d77dd000000000000000000000000000000001777499a4771221221221774a9947771
0000000000000000015d675715dd5115d567d510d7dddddddddddddddddddd7d0000000000000000000000000000000015665577511244244244211577556671
000000000000000015d6776555676d5656667d51d7dddddddddddddddddddd7d000200000006000000000000000000001566567d5661241441421665d7656671
0000000000000000156d677d567666d7d5d66751dddddddddddddddddddddddd0028200000000000000000000000000044556555666612244221666655565544
0000000000000000016767d356d677653d55dd10d7dddddddddddddddddddd7d00020000000000000000000000000000aa7d5dddddddd111111dddddddd5d7aa
00000000000000001556dd33d567765333d56751d7dddddddddddddddddddd7d0000000000000000007000000000000099d75d11116666666666661111d57d99
0000000000000000157dd3333d555533333d5651dddddddddddddddddddddddd0000000000000000000000000000000099dd5d1dd56666666666665dd1d5dd99
000000000000000011567533333333333d555111d7dddddddddddddddddddd7d0000000000000000000000000060000044556d1d5666666666666665d1d65544
00000000000000001115d65333333333d5665511d7dddddddddddddddddddd7d0070000000000000000000000676000015666d1566666dddddd6666651d66651
0000000000000000115d67653333333356d76d51dddddddddddddddddddddddd0000000000000000000000000060000015666d666666dddddddd666666d66651
0000000d7000000015566775333333335d667651d7dddddddddddddddddddd7d0000000000000000000000000000000021216d66666dddddddddd66666d61212
0000000d6000000015d76675333333335d666651d7dddddddddddddddddddd7d0000000010000000000000000000000042421d6666dddddddddddd6666d12424
00000a4d69a0000056167d65333333335dd76517dddddddddddddddddddddddd00000000000000000000000000000000424421666dddddddddddddd666124424
00006a1494a600005d61665d3333333335d651ddd7dddddddddddddddddddd7d00000000000000700000001000000000212121666dddddddddddddd666121212
0000591564950000155555d333333333335d6511d7dddddddddddddddddddd7d00000000000000000000000000000000424441666dddddddddddddd666144424
0000695154960000157dd3333d555533333d5651dddddddddddddddddddddddd01111110333333333333333366666666424441666dddddddddddddd666144424
00005954949500001556dd33d567765333d56751d7dddddddddddddddddddd7d1151dd11333333333333333366666666212121666dddddddddddddd666121212
0000694544960000116767d356d677653d556610d7dddddddddddddddddddd7d15157dd1333333333333333366666666424421666dddddddddddddd666124424
0000089459800000156d677d567666d7d5667751dddddddddddddddddddddddd115667d133333355333333336666666642421d6666dddddddddddd6666d12424
000000144100000015d6776555676d165d666d51d7dddddddddddddddddddd7d1d1665513333353b5d5333336666666621216d66666dddddddddd66666d61212
0000000000000000165d675715dd5175d5d6d510d7dddddddddddddddddddd7d11d1655133335db3b3b533336666666615666d666666dddddddd666666d66651
0000000000000000dd6511dd1155166755115100dd77d77d77d77d77d77d77dd111d15113335dd3b7b3d33336666666615666d1566666dddddd6666651d66671
00000000000000001dd11111111111d111110000dddddddddddddddddddddddd0111111033315d73b3b533336666666644556d1d5666666666666665d1d65544
000000003333333335333553dddddddd1111111111111111ddddddd1d0addddddddddddd3315d5373b3d33336666666699dd5d1dd56666666666665dd1d5dd99
000000003333333333335675ddd77ddd111111111111111ddddddd11d00ddddddd101ddd33115d5d3d3533336666666699d75d11116666666666661111d57d99
00000000333533333333d565ddd77ddd11111111111111ddddddd111da0dddddddd101dd311115d5d313333366666666aa7d5dddddddd111111dddddddd5d7aa
0000000033d7633336333d5dddd77ddd1111111111111ddddddd1111daaddddddddd101d33111151513333336666666644556555666612244221666655565544
00000000333d3333576333d3ddd77ddd111111111111ddddddd11111d0addddddddd101d3311111113333333666666661566567d5661241441421665d7656651
0000000033333333d5333333ddd77ddd11111111111ddddddd111111d00dddddddd101dd33331113333333336666666615665577511244244244211577556671
000000005633335333336733ddd77ddd1111111111ddddddd1111111da0ddddddd101ddd3333333333333333666666661555499a4551221221221554a9945571
000000003533333333535d33dddddddd111111111ddddddd11111111daaddddddddddddd3333333333333333666666661111499a4112442442442114a9941111
00000000000000000000000033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000022220033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000022ee22033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
000000097000000002e77e2033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00000009a000000002e77e2033333333333883333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00000019a9a00000022ee22033333333338888333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
0000111994a500000022220033333333388888833333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00005119a49600000000000033333333888888883333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00001151549500000000000033333338888888888333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00005154949600000000000033333388888888888833333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00001145449500000009900033333388888888888833333300000000000000000000000000000000000000000000000000000000000000000000000000000000
0000001559000000009a790033333338888888888333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000019aa90033333338888888888333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000119900033333333888888883333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000011000033333333888888883333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00000005600000000000000033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00000005700000000000000033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00550001500055000005600033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
05665105701566500056760033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
057695166d5967500056760033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00d195177d595d000015650033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00765d5886d567000056760033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00d15d5226d55d000000000033333333333333333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000
00765d5116d567000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00d15d1551d55d000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00769157765967000001100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0d519156765955d0001c710000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0677511551157760001cc10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0566510dd51566500001100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00565805508565000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00050000000050000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
0000010101000000000000000101010100000100010000000000000001010101000001010100000001000000010101010002020000000000000000000101010100000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000a0b00282828282828282828282828282828000000090a0a0b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00090a0008091a1b28020303030303030303030303030304280000191a1a1b000000000a0b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00191a00181900002812131313131313131305060606071428080900000000000000001a1b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000a0b00281213131313131313133738383817142818190000000008090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000001a1b0028121313131313131313373838381714280a0b0000000018190000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000090a0028121313131313131313252626262714281a1b00090a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000191a002812131313131313131313132323232428000000191a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000002812050606060606060606071428282800000809000000000b0000000008090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000a0b281215333333333333333317142800001a0018190000001a1b0000000018190000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000809001a1b28121516161616161616161714280b090000000000000b00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000181900000028121516161616161616161714281b090a0b0000001a1b00000809000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000028120c0d0e0f16360c0d0e0f14281a191a1b000000000000001819000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00090a000000000028121c1d1e1f16341c1d1e1f142800000008090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00191a00000a0b0028122c2d2e2f16342c2d2e2f1428000000181900000b08090000090a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000001a1b0028123c3d3e3f16343c3d3e3f14280809000000001a1b18190000191a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000281234343435163434343435142828282828282800000b00000000000008090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000a0b2812252626262626262626273103030303030304281a1b00000000000b18190000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000001a1b28121313131313434343434343434343434343430428000b0000001a1b0000000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000090a0000281213131313134343434343434343434343444314281a1b09000000000000091a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000191a080928121332311331323143434343292a434353545514280018190000000000001900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000181928223131323132314343434343393a434343434331042828282828282828280000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000002812133132324343434343434343434343434343430303030303030303042800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000a0b280231131332314343434343434343434343434343434313131313131332142800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
090a000000001a1b2812131313131343434343434343434343434343434343292a1313131313142800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
191a0000000000002812131313131343292a434343434343434343292a4343393a1313321313142800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000090a002812131313131343393a434343434343434343393a434332232323131313142800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000191a00282223233113131313131313131331232323232323232324282828223213142800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000002828282223311313131331232324282828282828282828000809281213142800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000a092828222323232324282828000000090a0000090a001819281213142800090a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000191a090a0028282828282800090a080900191a0000191a000000282223242800191a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000008191a0000000008090000191a1819000008090000000000090a2828280000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000191a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
