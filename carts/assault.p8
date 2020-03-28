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
	-- position in texmap space
	px,py=(px-48*sa*pz)-w,(py-48*ca*pz)-w
	local dx0,dy0=(sa-ca)*(w/2-0.5)+w,w-(ca+sa)*(w/2-0.5)

	-- dx/dy: offset in full texmap space (1024x1024)
	local mem,dx,dy=0x6000,8*ddx0,8*ddy0
	-- mdx: offset in texmap space (256x1024)
	local mdx1,mdx2,mdx3,mdx4,mdx5,mdx6,mdx7=shr(ddx0,3),shr(2*ddx0,3),shr(3*ddx0,3),shr(4*ddx0,3),shr(5*ddx0,3),shr(6*ddx0,3),shr(7*ddx0,3)
	for iy=0,127 do
		local srcx,srcy,mx=dy0+px,dx0+py,shr(dy0+px,3)
		for ix=0,15 do

			-- find pixel 8-pixel row				
			-- get x%8 pixel (starting at 8th nibble = 0xf000)
			-- shift back to least sig byte (int)
			poke4(
				mem,
				bor(
					bor(
						bor(rotr(band(shl(m[bor(band(mx,0xffff),band(lshr(srcy,16),0x0.ffff))],shl(band(srcx,7),2)),0xf000),28),
						rotr(band(shl(m[bor(band(mx+mdx1,0xffff),band(lshr(srcy-ddy1,16),0x0.ffff))],shl(band(srcx+ddx1,7),2)),0xf000),24)),
						bor(rotr(band(shl(m[bor(band(mx+mdx2,0xffff),band(lshr(srcy-ddy2,16),0x0.ffff))],shl(band(srcx+ddx2,7),2)),0xf000),20),
						rotr(band(shl(m[bor(band(mx+mdx3,0xffff),band(lshr(srcy-ddy3,16),0x0.ffff))],shl(band(srcx+ddx3,7),2)),0xf000),16))
					),
					bor(
					bor(rotr(band(shl(m[bor(band(mx+mdx4,0xffff),band(lshr(srcy-ddy4,16),0x0.ffff))],shl(band(srcx+ddx4,7),2)),0xf000),12),
					rotr(band(shl(m[bor(band(mx+mdx5,0xffff),band(lshr(srcy-ddy5,16),0x0.ffff))],shl(band(srcx+ddx5,7),2)),0xf000),8)),
					bor(rotr(band(shl(m[bor(band(mx+mdx6,0xffff),band(lshr(srcy-ddy6,16),0x0.ffff))],shl(band(srcx+ddx6,7),2)),0xf000),4),
					band(shl(m[bor(band(mx+mdx7,0xffff),band(lshr(srcy-ddy7,16),0x0.ffff))],shl(band(srcx+ddx7,7),2)),0xf000))
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
	local mask=shl(0xfff8,w/2)
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

function make_hitmask(sx,sy,sw,sh,tc)
	assert(flr(sw/32)<=1,"32+pixels wide sprites not yet supported")
	tc=tc or 0
	local bitmask={}
	for j=0,sh-1 do
		local bits=0
		for i=0,sw-1 do
			local c=sget(sx+i,sy+j)
			if(c!=tc) bits=bor(bits,lshr(0x8000,i))  
		end
		bitmask[j]=bits
	end
	return bitmask
end

-- in-memory sprite
-- includes support for rotated sprites
function make_memspr(sx,sy,w,h,nangles,tc)
	nangles=nangles or 1
	-- transparent color
	tc=tc or 0

	-- rotated versions
	local angles={}
	for i=0,nangles-1 do
		local a=i/nangles-0.25
		local spr=rspr(sx,sy,flr(w/8),a,tc)

		-- convert sprite to 8 pixel dword's
		local s,smask,hitmask,dw={},{},{},0		
		for y=0,h-1 do
			local mask,hitb,b,bmask=0x1000,0
			for x=0,w-1 do
				local c=spr[x+y*w]
				b=bor(b or 0,c*mask)
				bmask=bor(bmask or 0,(c==tc and 0xf or 0)*mask)
				-- warn: works only for up to 32 pixel wide sprites
				if(c!=tc) hitb=bor(hitb,lshr(0x8000,x))
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
			-- hitmask
			hitmask[y]=hitb
		end	

		angles[i]={s=s,smask=smask,hitmask=hitmask}
	end		

	-- default sprite
	local s,smask,hitmask=angles[0].s,angles[0].smask,angles[0].hitmask

	-- width in dword unit (e.g. texmap unit) +padding
	w=flr(w/8)+1

	return {
		rotate=function(self,angle)
			-- select sprite cache entry
			local cache=angles[flr(nangles*((angle%1+1)%1))]
			s,smask,hitmask=cache.s,cache.smask,cache.hitmask

			-- test

			self.hitmask=cache.hitmask
		end,
		draw=function(self,dst,x,y,blink)

			-- dword boundary
			local ix=flr(x/8)
			-- localize globals
			local w,s,smask,shift,dx=w,s,smask,shl(flr(x)-8*ix,2),ix
			local backup,bmask={},lshr(0xffff.ffff,shift)
			for j=0,h-1 do
				local srcx,dstx,v0mask,v0=j*w,bor(dx,shr(j+flr(y),16)),0xffff.ffff
				for i=0,w-1 do
					-- stride (shifted)
					local v1,v1mask=blink and 0x7777.7777 or rotr(s[srcx],shift),rotr(smask[srcx],shift)						
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

-- pick a value from an array
-- t must be [0,1[
function lerpa(a,t)
	return a[flr(#a*t)+1]
end

-- game
local time_t=0
local jumppads={}
local _map

-- todo: remove
local gravity=-0.03

function make_plyr(x,y,z,angle)
	local reload_ttl,reload_nuke_ttl,acc,da,dz,mortar_angle=0,0,0,0,0,0
	local states,state,nuke_mode
	local sprite=96
	local flip_sprites,mortar_sprites={
		[-1]={128,130,132},
		[1]={132,130,128}},
		{96,134,136}

	-- nuke velocity
	-- todo: remove
	local nuke_v=0.5
	
	-- shared fire bullet routine
	function fire_bullet(ca,sa)
		if btn(5) and reload_ttl<0 then
			make_part(16,56,0,x,y,z,-sa/2,-ca/2)
			reload_ttl=10
		end
	end

	function fire_nuke()
		if btn(5) and reload_nuke_ttl<0 then
			-- polar coords
			local cm,sm=nuke_v*cos(mortar_angle),-nuke_v*sin(mortar_angle)
			local ca,sa=cm*cos(angle),cm*sin(angle)
			make_part(16,48,1,x,y,z,-sa,-ca,sm)
			-- next nuke
			reload_nuke_ttl=15

			-- marker
			local a,b,c=gravity/2,sm,z-1
			local d=b*b-4*a*c
			if d>=0 then
				local t=(-b-sqrt(d))/a/2
				local marker=make_part(48,47,3,x-t*sa,y-t*ca,1)
				marker.ttl=20
			end
		end
	end

	-- create states
	states={
		drive=function()
			local ttl=0
			sprite,z,dz,nuke_mode=96,1,0
			return function(self)
				-- flip?
				if btn(4) then
					if(btn(0)) state=states.flip(-1) return
					if(btn(1)) state=states.flip(1) return
					if(btn(3)) state=states.mortar() return
				end

				-- regular drive
				if(btn(2)) acc+=0.05
				if(btn(3)) acc-=0.05
			
				-- rotation
				if(btn(0)) da=-0.01
				if(btn(1)) da=0.01
				
				angle+=da
				local ca,sa=cos(angle),sin(angle)

				local dx,dy=-sa*acc,-ca*acc
				
				-- get tile
				self.x,self.y=x,y
				local xarea,yarea=get_area(self,dx,0),get_area(self,0,dy,0)
				-- solid?
				if band(xarea,0x1)==0 then
					x+=dx
				end
				if band(yarea,0x1)==0 then
					y+=dy
				end
				-- gravel?
				local area=bor(xarea,yarea)
				if band(area,0x2)>0 and abs(acc)>0.1 then
					cam_shake()
				-- jumppad
				elseif band(area,0x4)>0 then
					local i=bor(flr(x),shl(flr(y),7))
					local j=jumppads[i]
					-- actvivate
					if j and j>0 then
						dz=0.05
						state=states.airborne()
						-- 
						jumppads[i]-=1
					end
				end

				fire_bullet(ca,sa)

				-- friction
				da*=0.8
				acc*=0.7
			end
		end,
		flip=function(dir)
			local ttl,sprites=20,flip_sprites[dir]
			local ca,sa=cos(angle),sin(angle)
			local dx,dy=0.1*ca*dir,-0.1*sa*dir
			-- stop fwd & rotation
			acc,da=0,0
			return function(self)
				ttl-=1
				if(ttl<0) state=states.drive() return

				sprite=lerpa(sprites,ttl/20)
				-- get tile
				self.x,self.y=x,y
				local xarea,yarea=get_area(self,dx,0),get_area(self,0,dy,0)
				-- solid?
				if band(xarea,0x1)==0 then
					x+=dx
				end
				if band(yarea,0x1)==0 then
					y+=dy
				end

				fire_bullet(ca,sa)
			end
		end,
		airborne=function()
			local ttl=0
			mortar_angle,nuke_mode=0,true
			return function()
				if(btn(0)) da=-0.01
				if(btn(1)) da=0.01
				
				angle+=da
				z+=dz
				dz-=0.001
				if z<1 then
					state=states.drive()
					return
				end
						
				fire_nuke()

				-- friction
				da*=0.9
				acc=0
			end
		end,
		mortar=function()
			nuke_mode=true
			-- stop fwd & rotation
			acc,da,mortar_angle=0,0,0.02
			return function()
				if(mortar_angle<0.02) state=states.drive() return

				if(btn(3)) mortar_angle+=0.01				
				mortar_angle=mid(mortar_angle*0.95,0,0.2)				
				sprite=lerpa(mortar_sprites,mortar_angle/0.2)

				fire_nuke()
			end
		end,
		drop=function()
			local bounces=2
			return function()
				z+=dz
				dz-=0.001
				if z<1 then
					bounces-=1
					z=1
					if bounces>0 then
						dz=abs(dz)/4
					else
						state=states.drive()
					end
				end
			end
		end
	}

	-- initial state
	state=states.drop()

	return {
		w=0.4,
		h=0.4,
		get_pos=function()
			return x,y,z,angle
		end,
		draw=function(self)
			-- player
			spr(sprite,56,104,2,2)

			if nuke_mode then
				-- nuke estimated impact marker
				-- x=v*cos(mortar)*t
				-- y=0.5*g*t^2+v*sin(mortar)*t+y0
				local a,b,c=gravity/2,-nuke_v*sin(mortar_angle),z-1
				local d=b*b-4*a*c
				if d>=0 then
					local t=(-b-sqrt(d))/a/2
					local sy=nuke_v*cos(mortar_angle)*t
					local dx,dy=64,112-shl(sy,3)
					if(time_t%2==0) sspr(48,32,13,13,dx-6.5,dy-6.5)

					--[[
					for i=0,10 do
						local tt=(i*t)/10
						local xx=nuke_v*cos(mortar_angle)*tt
						local yy=0.5*gravity*tt*tt-nuke_v*sin(mortar_angle)*tt+z
						circfill(64,112-shl(xx,3),yy,7)
						print(yy,68,112-shl(xx,3),yy<1 and 8 or 7)
					end
					]]
				end
			end
		end,
		update=function(self)
			reload_ttl-=1
			reload_nuke_ttl-=1
			state(self)

			-- kill "ghost" rotation
			if(abs(da)<0.001) da=0
		end
	}
end
local plyr=make_plyr(28,22,2,0)

-- in memory map of sprite coords
local _texmap={}

-- check for the given tile flag
function fmget(x,y)
	return fget(_map[flr(x)+128*flr(y)])
end
-- return true if solid tile
function solid(x,y)
	return fget(_map[flr(x)+128*flr(y)],0)
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

local parts={}
function update_parts(m)
	for i,p in pairs(parts) do
		p.t+=1
		-- elapsed?
		if(p.ttl and p.t>=p.ttl) parts[i]=nil break

		-- bullet
		if p.kind==0 then
			local x0,y0=p.x,p.y
			local x1,y1=x0+p.dx,y0+p.dy
				-- hit wall?
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
			p.dz+=gravity
		elseif p.update then
			parts[i]=p:update()
		end
	end
end
function draw_parts(m,x0,y0,z0,angle)
	local ca,sa=cos(angle),sin(angle)
	for _,p in pairs(parts) do
		-- actual "depth"
		local z=p.z-z0+1
		-- front of cam?
		if true then --z>0.1 then
			local x,y,w=(p.x-x0)/z0,(p.y-y0)/z0,max(1,p.w*z)
			-- project
			local dx,dy=64+shl(ca*x-sa*y,3),112+shl(sa*x+ca*y,3)
			if p.draw then
				p:draw(dx-w/2,dy-w/2,z0)
			elseif p.kind==3 then
				local sx,sy,w=48,47,17
				if(time_t%8<4) sx,sy,w=61,32,13
				sspr(sx,sy,w,w,dx-w/2,dy-w/2)
			else
				-- todo: select rotated sprite
				sspr(p.sx,p.sy,p.w,p.w,dx-w/2,dy-w/2,w,w)
			end
		end
	end
end

local circles={
 {{r=8,c=7}},
 {{r=6,c=0}},
 {{r=5,c=2},{r=4,c=9},{r=3,c=10},{r=2,c=7}},
 {{r=7,c=2},{r=6,c=9},{r=5,c=10},{r=4,c=7}},
 {{r=8,c=0},{r=6,c=2},{r=5,c=9},{r=3,c=10},{r=1,c=7}},
 {{r=8,c=0,fp=0xa5a5.ff,fn=circ}},
 {{r=8,c=0,fp=0x5a5a.ff,fn=circ}},
 {{r=8,c=0,fp=0x5a5a.ff,fn=circ}}
}
 
function draw_blast(p,x,y)
	local t=p.t/p.ttl
	palt(0,false)
	palt(14,true)
	local cc=circles[flr(t*#circles)+1]
	for i=1,#cc do
		local c=cc[i]
		if(c.fp) fillp(c.fp)
		(c.fn or circfill)(x,y,c.r,c.c)
		fillp()
	end
	palt()
end

function make_part(sx,sy,kind,x,y,z,dx,dy,dz)
	return add(parts,{
		-- age
		t=0,
		sx=sx,
		sy=sy,
		kind=kind,
		x=x,
		y=y,
		z=z,
		h=8,
		w=8,
		dx=dx or 0,
		dy=dy or 0,
		dz=dz or 0,
		-- todo: only for relevant particles
		hitmask=make_hitmask(sx,sy,8,8)
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

function make_nuke(x,y)
	local r0,r1,fp,t=8,8,#dither_pat,0
	-- nuke always at floor level
	local nuke=make_part(0,0,2,x,y,1)
	nuke.update=function(self)
		t+=1
		if(t>27) return
		return self
	end
	nuke.draw=function(self,x,y,z)
		--
		if t<4 then
			fade(t)
		else
			pal()
			r0=lerp(r0,45,0.22)/z
			r1=lerp(r1,45,0.3)/z
			local rr0,rr1=r0*r0,r1*r1
			if(t>20) fp=lerp(fp,1,0.1)	
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

local npcs={}
local turret=make_memspr(96,32,32,32,32)
local tank=make_memspr(0,32,16,16,16)
local blast=make_memspr(80,32,16,16,nil,14)

function array_add(a,value)
	local tmp={}
	if a then
		for k,v in pairs(a) do
			tmp[k]=v
		end
	end
	add(tmp,value)
	return tmp
end

function decompress(mem,fn)
	local code,code_len,code_bits={},256,8
	for i=0,255 do
		code[shr(i,16)]={i}
	end
    local buffer,buffer_bits,index,prefix=0,0,0

	local x,y=0,0
	local len=peek2(mem)
	mem+=2
    while index < len or buffer_bits >= code_bits do
        -- read buffer
        while index < len and buffer_bits < code_bits do
			buffer=bor(shl(buffer, 8),shr(peek(mem+index),16))
        	buffer_bits += 8
			index += 1
			--print(tostr(buffer,true))
		end
        -- find word
        buffer_bits -= code_bits
        local key = lshr(buffer,buffer_bits)
		buffer = band(buffer,shl(0x0.0001,buffer_bits)-0x0.0001)
		local word = code[key]
		if(not word) word=array_add(prefix, prefix[1])

        -- store word
		if prefix then
			code[shr(code_len,16)]=array_add(prefix, word[1])
			code_len+=1
		end
		-- print(index.." k:"..tostr(key,true).." #w:"..#word.." ("..found..")")
		prefix = word
		--print(index..": "..#word)
		--if(index>950) stop()

        -- code length
		if code_len >= 2^code_bits then
			-- print(#code.." bits: "..(code_bits+1))
			code_bits += 1
		end
		-- emit output
		-- total_len+=#word
		
		for i=1,#word do
			local s=word[i]
			if(s!=0) fn(s,x,y)
			x+=1
			if(x>127) x=0 y+=1 
		end
	end
end
	
function _init()
	_map={}
	decompress(0x2000,function(s,i,j)
		_map[i+128*j]=s

		if fget(s,2) then
			jumppads[i+128*j]=3
		end
		
		local sx,sy=band(shl(s,3),127),shl(flr(s/16),3)
		for y=0,7 do
			local b=0
			for shift=0,7 do
				b=bor(b,sget(sx+shift,sy+y)*lshr(0x1000.0000,4*shift))
			end
			-- use x.y 16:16 for texmap coords
			_texmap[bor(i,lshr(8*j+y,16))]=b
		end
	end)

	-- test particules
	for i=1,20 do
		add(npcs,{x=20+rnd(10),y=57+rnd(10),h=16,w=16,spr=tank,cache={}})
	end
	-- turrets
	add(npcs,{x=21,y=33,h=32,w=32,spr=turret,cache={}})
	add(npcs,{x=28,y=33,h=32,w=32,spr=turret,cache={}})
end

function _update()
	time_t+=1
	cam_update()

	plyr:update()

	update_parts(_texmap)
end

function intersect_bitmasks(a,b,x,ymin,ymax)
	local by=flr(8*a.y)-flr(8*b.y)
	for y=ymin,ymax do
	 -- out of boud b mask returns nil
	 -- nil is evaluated as zero :]
		if(band(a.hitmask[y],lshr(b.hitmask[by+y],x))!=0) return true		
	end
end

function collide(a,b)
	-- a is left most
	if(a.x>b.x) a,b=b,a
	-- screen coords
	local ax,ay,bx,by=flr(8*a.x),flr(8*a.y),flr(8*b.x),flr(8*b.y)
	local xmax,ymax=bx+b.w,by+b.h
	if ax<xmax and 
	 ax+a.w>bx and
	 ay<ymax and
	 ay+a.w>by then
	 -- collision coords in a space
 	return true,a,b,bx-ax,max(by-ay),min(by+b.h,ay+a.h)-ay
	end
end


local red_blink={0,1,2,2,8,8,8,2,2,1}

function draw_bitmask(bits,x,y,c)
 color(c)
 local ix=32*flr(x/32)
 for j,b in pairs(bits) do
  -- shift to x
 	b=lshr(b,x%32)
 	for i=0,31 do
 		if(band(lshr(0x8000,i),b)!=0) pset(ix+i,y+j,c)
  end
 end
end

local blasts={}
function _draw()

	local px,py,pz,pangle=plyr:get_pos()

	for i=#npcs,1,-1 do
		local npc=npcs[i]
		restoremap(npc.cache,_texmap)

		-- select correct hitmask
		npc.angle=atan2(npc.x-px,-npc.y+py)
		npc.spr:rotate(npc.angle)

		npc.hitmask=npc.spr.hitmask
	end

	-- commit blasts to texmap
	for _,b in pairs(blasts) do
		blast:draw(_texmap,8*b.x,8*b.y)
	end
	blasts={}

	local blink={}
	for p in all(parts) do
		-- todo: align box with display position!
		local blt={x=p.x-0.5,y=p.y-0.5,w=8,h=8,hitmask=p.hitmask}
		for npc in all(npcs) do
			local col,a,b,x0,y0,y1=collide(blt,npc)
			if col and intersect_bitmasks(a,b,x0,y0,y1) then
				--blink[npc]=true
				del(npcs,npc)
				del(parts,p)
				local b=make_part(0,0,0,npc.x+npc.w/16,npc.y+npc.h/16,1)
				b.draw=draw_blast
				b.ttl=10

				-- blast crater
				add(blasts,{x=npc.x,y=npc.y})
				break
			end
		end	
	end

	-- draw
	for i=1,#npcs do
		local npc=npcs[i]
		npc.spr:rotate(npc.angle)
		npc.cache=npc.spr:draw(_texmap,8*npc.x,8*npc.y,blink[npc])
	end

	--[[
	restoremap(c0,_texmap)
	turret:rotate(atan2(11.5-plyr.x,-12.5+plyr.y)+0.5)

	local col,a,b,x0,y0,y1,blink
	for _,p in pairs(parts) do
		-- todo: align box with display position!
		col,a,b,x0,y0,y1=collide({x=p.x-0.5,y=p.y-0.5,w=8,h=8,hitmask=p.hitmask},{x=10,y=11,w=32,h=32,hitmask=turret.hitmask})
		if col and intersect_bitmasks(a,b,x0,y0,y1) then
			blink=true
			break
		end	
	end
	]]

	-- rotating map
	rsprtexmap(_texmap,8*px+shkx,8*py+shky,pz,pangle)	

	plyr:draw()

	draw_parts(_texmap,px,py,pz,pangle)

	--[[
	if col then
		draw_bitmask(a.hitmask,0,0,7)
 		draw_bitmask(b.hitmask,x0,flr(8*b.y)-flr(8*a.y),7)
	end
	]]
	
	--line(64,64,64+16*ca,64+16*sa,11)
	--line(64,64,64-16*sa,64+16*ca,8)

	-- blinking lights
	pal(8,red_blink[flr((5.3*time())%#red_blink)+1],1) 

	--[[
	prints("score",2,2,14,1)

	local s=tostr(flr(time()))
	prints(s,30-#s+1,9,7,0)
	]]

	rectfill(0,0,127,8,1)
	print(stat(1).."/"..stat(7).."/"..stat(9).." "..stat(0).."kb",2,2,7)
end

function prints(s,x,y,c,sc)
	if(sc) print(s,x+1,y+1,sc)
	print(s,x,y,c)
end

__gfx__
0000000033333333000000055ddddd5511000000dddddddddddddddddddddddd000000000000000000000000000000001111499a4112442442442114a9941111
0000000035333333000001ddd66666dd55115100dd77d77d77d77d77d77d77dd000000000000000000000000000000001777499a4771221221221774a9947771
00000000333333330000155566666666d567d510d7dddddddddddddddddddd7d0000000000000000000000000000000015665577511244244244211577556671
0000000033333333000151567776666656667d51d7dddddddddddddddddddd7d000200000006000000000000000000001566567d5661241441421665d7656671
000000003333353300155567dddddd15d5d66751dddddddddddddddddddddddd0028200000000000000000000000000044556555666612244221666655565544
000000003353333300155dd65d1155155555dd10d7dddddddddddddddddddd7d00020000000000000000000000000000aa7d5dddddddd111111dddddddd5d7aa
0000000033333333015555615115555111556751d7dddddddddddddddddddd7d0000000000000000007000000000000099d75d11111111111111111111d57d99
0000000033333333105156111551111111155651dddddddddddddddddddddddd0000000000000000000000000000000099dd5d1dd51111111111115dd1d5dd99
33333333333333331551d6d5333333335551d655d7dddddddddddddddddddd7d0000000000000000000000000060000044556d1d5111111111111115d1d65544
33333333333333331115d66d333333335115d6d5d7dddddddddddddddddddd7d0070000000000000000000000676000015666d15111115555551111151d66651
33331652163333331115d76d333333331115d765dddddddddddddddddddddddd0000000000000000000000000060000015666d11111155555555111111d66651
333156d817633333151d766d33333333151d7665d7dddddddddddddddddddd7d0000000000000000000000000000000021216d11111555555555511111d61212
3315d75257663333155d666d33333333155d66d5d7dddddddddddddddddddd7d0000000010000000000000000000000042421d11115555555555551111d12424
3155d77577776333155d666533333333155d6dd5dddddddddddddddddddddddd0000000000000000000000000000000042442111155555555555555111124424
3111166667511333151176d53333333315117d55d7dddddddddddddddddddd7d0000000000000070000000100000000021212111155555555555555111121212
11288d66658823331555d765333333335555d755d7dddddddddddddddddddd7d0000000000000000000000000000000042444111155555555555555111144424
11111666675d53330151d6d33dddd53311d666d1dddddddddddddddddddddddd0111111033333333333333336666666642444111155555555555555111144424
115dd56d6777633301556dddd66666dddd6667d1d7dddddddddddddddddddd7d1151dd1133333333333333336666666621212111155555555555555111121212
1115dd58576613330151d76d6676666666667d51d7dddddddddddddddddddd7d15157dd133333333333333336666666642442111155555555555555111124424
311155d816d133330015556dddd7666666dd5510dddddddddddddddddddddddd115667d133333355333333336666666642421d11115555555555551111d12424
331115d21d13333300155155155ddd151555d100d7dddddddddddddddddddd7d1d1665513333353b5d5333336666666621216d11111555555555511111d61212
3331111111333333000115155111551551155100d7dddddddddddddddddddd7d11d1655133335db3b3b533336666666615666d11111155555555111111d66651
3333333333333333000001115115555515551000dd77d77d77d77d77d77d77dd111d15113335dd3b7b3d33336666666615666d15111115555551111151d66671
3333333333333333000000005511111101110000dddddddddddddddddddddddd0111111033315d73b3b533336666666644556d1d5111111111111115d1d65544
dddddddd3333333335333553dddddddd1111111111111111ddddddd1d0addddddddddddd3315d5373b3d33336666666699dd5d1dd51111111111115dd1d5dd99
dddddd7d3333333333335675ddd77ddd111111111111111ddddddd11d00ddddddd101ddd33115d5d3d3533336666666699d75d11111111111111111111d57d99
ddddd77d333533333333d565ddd77ddd11111111111111ddddddd111da0dddddddd101dd311115d5d313333366666666aa7d5dddddddd111111dddddddd5d7aa
dddd7d7d33d7633336333d5dddd77ddd1111111111111ddddddd1111daaddddddddd101d33111151513333336666666644556555666612244221666655565544
dddd7d7d333d3333576333d3ddd77ddd111111111111ddddddd11111d0addddddddd101d3311111113333333666666661566567d5661241441421665d7656651
ddddd77d33333333d5333333ddd77ddd11111111111ddddddd111111d00dddddddd101dd33331113333333336666666615665577511244244244211577556671
dddddd7d5633335333336733ddd77ddd1111111111ddddddd1111111da0ddddddd101ddd3333333333333333666666661555499a4551221221221554a9945571
dddddddd3533333333535d33dddddddd111111111ddddddd11111111daaddddddddddddd3333333333333333666666661111499a4112442442442114a9941111
00000000000000000000000033333333333333333333333300007777700000000888880000000000eeeeeeeeeeeeeeee00000000000000000000000000000000
000000000000000000222200333333dddddddddddd33333300007000700000000088800000000000eee0ee00e0e0eeee00000000000000000000000000000000
0000000d70000000022ee22033333357277777727d33333300000000000000000008000000000000eeeee08500eeeeee00000000000000000000000000000000
0000a906709a000002e77e20333335d82666666827d3333300000000000000000000000000000000ee0ee052800ee0ee00000000000000000000000000000000
0009d421524d900002e77e20333335eee666666e82d3333377000000000778000008000008000000eeee042014000eee00000000000000000000000000000000
000d45f67f54d000022ee22033335d6666666666667d333370000000000078800080800088000000eee04d60010040ee00000000000000000000000000000000
000556ffff6550000022220033335d6666666666667d333370000070000078880800080888000000e0e085d8024da0ee000000000000005d7600000000000000
000d59faaf95d000000000003335d666666226666667d33370000000000078800080800088000000ee0880820285d00e00000000000055d66776000000000000
000559f99f955000000000003335d666662882666667d33377000000000778000008000008000000ee04d000002800ee0000000000055d666777600000000000
000d549dd945d00000000000335d66666288882666667d3300000000000000000000000000000000e005a808d002821e00000000007777777777770000000000
000554922945500000099000335d66662888888266667d3300000000000000000008000000000000ee014005940001ee000000000d6666666666667000000000
0004424994244000009a79003126666e88888888266662d300007000700000000088800000000000eeee100250280eee000000000d6666666666667000000000
0002822222282000019aa9003182666e88888888266628d300007777700000000888880000000000eeeee0080082eeee000000005dd666666666667600000000
000011111111000001199000318e6666e88888826666e8d300000000000000000000000000000000ee0ee100000e0eee00000005ddd666666666667760000000
00000000000000000011000031e66666e888888266666ed300000000000000000000000000000000eeeeee01e0eeeeee0000000dddd666666666667770000000
000000000000000000000000331d66666e8888266666d53300000088888000000000000000000000eeeeeeeeeeeeeeee000000dd5dd666666666676777000000
0000000000000000000000003331d6666eeeeee6666d5333000000088800000000000000336667766776677667766633000000555dd666666666676666000000
00000000000000000000000033331d666666666666d533330000000080000000000000003676677dd776d776d7766763000000155dd66666666667666d000000
0000000d6000000000056000333331d6666666666d533333000000000000000000000000d77111111111111111111776000000115dd666666666676dd5000000
0000090d70900000005676003333331d66666666d533333300000000000000000000000016d11dd11dd11dd11ddd11d6000000011ddd66666666766d50000000
00009a9d69a900000056760033333331d666666d5333333300000000000000000000000066ddddddddddd4adddddd166000000011ddd66666666766550000000
00006df5dfd6000000156500333333331d6226d53333333380000088888000008000000077dddddddddd149dddddd177000000001dd555555555d66500000000
0000d596795d00000056760033333333312882533333333388000080008000088000000077dddddddddd11dddddd1177000000000d56d556d5565d6000000000
00006df88fd6000000000000333333333311113333333333888000800080008880000000d6dddddddddddddddddd11d60000000005167516751675d000000000
00056df5dfd6500000000000dddddddddddddddddddddddd88000080008000088000000066ddddddddddddddddddd16600000000001670167016700000000000
000fd596795df00000000000d777777dddddddddd7dddddd80000088888000008000000077ddddddddddddddddddd17700000000001490149014900000000000
000f6d4664d6f000000cc000dd7dd7ddddddddddd77ddddd00000000000000000000000077dddddddddddddddddd117700000000001490149014900000000000
00096d49f9d6900000c7cc00ddd77dddddddddddd7d7dddd000000000000000000000000d6dddddddddddddddddd11d6000000000014a014a014a00000000000
00049f1491f9400001cc7c00ddddddddddd77dddd7d7dddd00000000000000000000000066ddddddddddddddddddd16600000000000000000000000000000000
0001481111841000011cc000dddddddddd7dd7ddd77ddddd000000008000000000000000677dddddddddddddddddd77600000000000000000000000000000000
000011000011000000110000ddddddddd777777dd7dddddd0000000888000000000000001d76677667766776677667d100000000000000000000000000000000
000000000000000000000000dddddddddddddddddddddddd000000888880000000000000115d677dd776d776d776d51100000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000d666d66dd66d666732233223233322333443344343334433
000000000000000000000000000000000000000000000000000000000000000000000000000000005d666660066666762bb22222222222224334444444444443
0000000d6f0000000000000d600000000000000d600000000000000000000000000000000000000055d666666666676613b22bb2232222223334433443444444
0000009d7900000000000004900000000000009d7900000000000000000000000000000000000000555dddddddddd666112213b2332bb2233333433433433443
000009a96a9000000000004994000000000009a69a90000000000000000000000000000000000000555766500766d666322211222213b2233444444444333443
000006dfdd6000000000005d75000000000006ddfd60000000000101101000000000000000000000555765500776d666322bb222221122233443344444334443
00000d5975d00000000000d56d00000000000d5795d0000000001915d19100000000000110000000555155d66a77d6663213b2bb222233223433343344443344
000006df8d6000000000005675000000000006d8fd60000000009a9d69a900000000010110100000d05100600600d60d2211213b222323224433433344434344
000056dfdd650000000000d565000000000056ddfd65000000006df5dfd600000000111111110000d05100600600d60d2bb22112222222224334433444444444
0000fd5975df00000000005f7f0000000000fd57955f00000000d596795d00000000141d61410000555155d66a77d66613b22222222332223334444444433444
0000f6d46d6f0000000000df6f0000000000f6d64d6f0000000d6df5dfd6d00000014a4664a410005551155005d6d66d11223332222333233344333444433343
000096d4fd69000000000059d9000000000096df4d6900000006d5f28f5df000000d5ff5dff5d000555711500566d666222223322bb223224444433443344344
000049f49f94000000000024f4000000000049f94f94000000096d4674d6900000096d45d9d69000555771111177d66d322bb22213b222223443344433344444
00000481184100000000001241000000000014810840000000019f1491f9100000019f1281f910005555555555555d663213b222112222233433344433444443
0000001011100000000000011000000000000110000000000000110110110000000011011011000055555550055555d622112322222222224433334444444444
000000000000000000000000000000000000000000000000000000000000000000000000000000005525255dd555555d32333322233332333433334443333433
00090000000900000009000033333333333333333333333333333333335bbb33113333333333333133333311333333111111111111111111dd566666677776dd
009a90000d6f7d000d9a9d00333333333333333333b5bb333333333335b5bbb3113333333333331133333311333333111111111111111113d56ddd555555576d
d66f76d0766f767076697670333bb3333333333335577bb33333333355577bbb33333333333333113333531133333333333333333333333316d0000000000576
766f7670006f7000006f700033566b333333333335d667b33333333355d887bb3333333333333311333333113333333353333333533335331600000000000057
006f700000676000006f7000335d6b333333333335d667b33333333355d887bb3333333333333311333333113333333333533333333333331600000000000057
006f7000000f000000d6d0003335533333333333355ddbb333333333555ddbbb3333333333333311333533113333333333333333333333331600000000000057
006f7000000000000000000033333333333333333355b533333333333555b5b33333333333333311333333113333333333333533533333331600000000000057
0006000000000000000000003333333333333333333333333333333333555b333333333333333311333333113333333333333333333333331600000000000056
33333333333333330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001600000000000056
3333315555dd53330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001600000000000056
33331555d567d53300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000016000000000000d6
3331515656667d5300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000016000000000000d6
33155567d5d667d30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001600000000000066
33155dd65555ddd300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000017d0000000000661
315555611155d753000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000d17666666666661d
1551561111155653000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000dd111111111111dd
1151d6d011d666530000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11556ddddd6667530000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1151d76d666676530000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1115556d666d66530000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
311551551666d5330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3111151557d665330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
331111111dd653330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
333111110d6533330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
0000010101000000000000000000000000000100010000000000000000000000000001010100000001000000000000000002020000000000000000000000000000000000000000000000000000000000000000000400000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101000000000000000000000000000001010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
960600804060503824160d0784426150b864361d0f8844625138a4562d178c466351b8e4763d1f904864523924964d27944a6552b964b65d2f984c665339a4d62e0a0c82c3408040666d340c86c3409050683418a150c320804cf0101b0cd22833f96820350168566b55bae56ea92dad0080763b2596cd6402340315f94342c5
6360ab2e573ba5d6e4aa0a342af6c93340257300e040213c26170d86bc340377cbe84b0801476451f83c20140d97cc0180e13c48271925bf04f209ed227f28131b8e355aa0b0582f9cbc82f3f24d0e411c8fdbe9f53ab1c6b75f89d96ce45b5472793fc6dd6f37baed862b85c3c76db71a7098944dd7ec0984fcde0f3e3c0bd0
e8b9187f2611602311891a19eef47833e1d1753cb85bcb4201ddf6c6c30d01108d63828020079de87a024400095adf94741a7817e65999664070495c0000b50a0a4800a5f8150ce1d87a1e05e1302c0888d475ee184780986dad8b22d885eb069458a12206a2b8b62e84d3989e3347def048158de38341568f12286a3f9062c8
be329152078248925ae048009324d77d7e0300d0380f6b4369665b84c0895a195f81c0741e07dad0d2669a21302a6347e350482c0b42e0be6a9d27684dec9c11c06d7e0f03d0f83f9aa81a0e130627c4341b5200b5158b9f5029fc120d2950d66aa54340d66e4380aa30185180906c089567d8c6369262f9890b4ea904181a06
c0b025414e6450663e759d97642784e3b41c184f8005bd665a55d566178a01b8a812806012aa6eaf90402d6b589715d8c059d635a400a29ed5597e7c985006cf3400ab990851800048ad61410045852ad765c8c05a6dd77aa763813bbaedbbee4066e941ecaba9860802161d91239885e400ab9ed06a3e6130561712b92dc062
8902a0802a245580967af97909f2389fc909ec8d85bd0d0b05f9679f0794abca505b4d4e54f206188f27c8f23dc6c8da4611899be0a78002305f361ad859c0481c0a8593ebe443110456149ecef26cef3a27d84ca70d73d6ed1746bc2f2bd105be453150557933dd589eca169d719f0232ed8346d2166d7c6319065dcdc66158
9aadcfdc736bed84e0c010c432e23870c43104ef15d6ec61ee1d8329cadb37ef36c4b11c1b86e1f88e7b8bd820179a04e90b161ee4dfdb34f822e9ba1e7832e2b8b0cba1609e67a95dcd99ca71deeafad7cf9ce339de7746801842a6130014c064099cba7f211455b6f48e90eb3a10c58104fc2ecde5b35cd8c6a053772c5508
f323a062a406fe9b9b19f4922827b9f1013ec387effa27031bace93f39f6bd8000613a58c8516012927a001eab7331eebdb9bd839a4e5121457c4a20fc1007d2000b72d92d0ca9f69215fe401f83bf7b2fd0f982905469cceab7446b2cc3b305eab4a0b3445e45cd94a0c7524910434481063cd13a03e60e41d40c33a02d5920
c3e069d98902566d7a04331794ff52bc1f788e49c8b0a4860658cb0f3c3031dd2e5890b09afc4b58ac3118b0f24267a1c43978b0221233f36206d7300b6207c4ca19d203162281f35b0b14aca545ce8a5b93b48b4d821f46c654a810b38131f032162432005ad9746b30d240c20ac6eb1855b91d8cd17e05c698732118c2b18e
3271949564c519cc248330d2a1b0af3006b1558a7e8ff1464e43931318de5a4f5c5205b19b2963274c22f18f234209917971020dcbdd87279d0300951c8ea22bb530a84cacca68d1148c4a09235315b032464531daa09e8a404d019ea6180609c23148e619ec4462c926a044d03c8625ca9183f701da31c5348ce99d1c632679
271c7b29e40a211ef9ed1c8ca38e72135640ba7362474f05053c8ce27d9913c73c4fa8009d0bd89dce97226427eb25649386831879246260d9154547f4f43be30c71a902e14067a6664c3206bfe3f801644c9991b23648ce285cb93e52aa794f32329bcad52aa59020583b73eca8c85bbd722d527d9c6678ce5aa52230b24a42
23147aa816f9fca92d1aa59f62a4c6ea2104317582a8b3c38b3e0e3d218732aa4213a24688d0d1fdac2e8d02bfca4e419501fc3fd5acd252e34d4fe5f340a6847ea354830b328adb4c22855ec052c59b42e98a04af3150a612628651cac81476c7d92a40322ad360b5106c158cae477908f2c95262a553915a9198dca9e953a4
b70e90f500a62eb4ec05b9b716eda0c0482d1ed8111b5a6a8ae2c01428a7a2b59db98576bebee518a92274c4548021604e7bac4f22b5c9a88f2d1d2235804e9a640e5237a6f55ebbd97b6f75efbe17c6f95f3be97d6fb5f7bf17e6fd5fbbf97f6ff5ffc0180701603c0981703607c118270560bc198370760fc2184709613c29
8570b617c318670d61bc398770f61fc4188711623c4846000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
