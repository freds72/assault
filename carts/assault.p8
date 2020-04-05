pico-8 cartridge // http://www.pico-8.com
version 19
__lua__
-- assault
-- by @freds72

-- misc helper functions
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

function normalize(u,v,scale)
	scale=scale or 1
	local d=sqrt(u*u+v*v)
	if (d>0) u/=d v/=d
	return u*scale,v*scale
end

-- manhattan distance (safe for overflow)
function dist(x0,y0,x1,y1)
	return abs(x1-x0)+abs(y1-y0)
end

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

-- coroutine helpers

-- delayed functions
local _futures,_co_id={},0
-- registers a new coroutine
function do_async(fn)
	_futures[_co_id]=cocreate(fn)
	-- no more than 64 co-routines active at any one time
	-- allow safe fast delete
	_co_id=(_co_id+1)%64
end
-- wait until timer
function wait_async(t,fn)
	for i=1,t do
		yield()
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

-- convert a sprite number to a sprite stucture
function make_sprite(s,sw,sh)
	return {spr=s,ssx=band(s*8,127),ssy=8*flr(s/16),sw=sw or 8,sh=sh or 8}
end
-- attach hitmask to sprite instance
function with_hitmask(sprite,tc)
	local sx,sy,sw,sh=sprite.ssx,sprite.ssy,sprite.sw,sprite.sh
	assert(sw<=32,"32+pixels wide sprites not supported")
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
	-- attach it
	sprite.hitmask=bitmask
	return sprite
end

-- returns true if hitmasks intersects
function intersect_bitmasks(a,b,x,ymin,ymax)
	local by=flr(a.sy)-flr(b.sy)
	for y=ymin,ymax do
	 -- out of boud b mask returns nil
	 -- nil is evaluated as zero :]
		if(band(a.hitmask[y],lshr(b.hitmask[by+y],x))!=0) return true		
	end
end

-- note: a/b must be in screen coords
function collide(a,b)
	-- a is left most
	if(a.sx>b.sx) a,b=b,a
	local ax,ay,bx,by=flr(a.sx),flr(a.sy),flr(b.sx),flr(b.sy)
	local xmax,ymax=bx+b.sw,by+b.sh
	if ax<xmax and 
		ax+a.sw>bx and
		ay<ymax and
		ay+a.sw>by then
	 	-- collision coords in 'a' space
 		return true,a,b,bx-ax,max(by-ay),min(by+b.sh,ay+a.sh)-ay
	end
end

-->8

-- game globals
local time_t=0
local jumppads={}
local _map,_cells,_cells_map,_grid,_map_lru

-- todo: remove for optimisation
local gravity=-0.03

-- player factory
function make_plyr(x,y,z,angle)
	local reload_ttl,reload_nuke_ttl,acc,da,dz,mortar_angle=0,0,0,0,0,0
	local states,state,nuke_mode
	local default_sprite=with_hitmask(make_sprite(96,16,16))
	local flip_sprites,mortar_sprites={
		[-1]={
			with_hitmask(make_sprite(128,16,16)),
			with_hitmask(make_sprite(130,16,16)),
			with_hitmask(make_sprite(132,16,16))},
		[1]={
			with_hitmask(make_sprite(132,16,16)),
			with_hitmask(make_sprite(130,16,16)),
			with_hitmask(make_sprite(128,16,16))}},
		{
			with_hitmask(make_sprite(96,16,16)),
			with_hitmask(make_sprite(134,16,16)),
			with_hitmask(make_sprite(136,16,16))
		}
	
	-- select default
	local sprite=default_sprite
	
	-- nuke velocity
	-- todo: remove
	local nuke_v=0.5
	
	-- player-specific particles
	-- regular bullet
	local bullet_cls=with_hitmask({
		ssx=16,ssy=56,
		sw=8,sh=8,
		kind=0,
		side=2
	})
	local nuke_shell_cls={
		ssx=16,ssy=48,
		sw=8,sh=8,
		kind=1
	}
	-- nuke marker
	local marker_cls={
		ttl=20,
		kind=3
	}

	function fire_bullet(ca,sa)
		if btn(5) and reload_ttl<0 then
			make_bullet(bullet_cls,x,y,1,-sa/2,-ca/2)
			reload_ttl=10
		end
	end

	function fire_nuke()
		if btn(5) and reload_nuke_ttl<0 then
			-- polar coords
			local cm,sm=nuke_v*cos(mortar_angle),-nuke_v*sin(mortar_angle)
			local ca,sa=cm*cos(angle),cm*sin(angle)
			make_part(nuke_shell_cls,x,y,z,-sa,-ca,sm)
			-- next nuke
			reload_nuke_ttl=15

			-- marker
			local a,b,c=gravity/2,sm,z-1
			local d=b*b-4*a*c
			if d>=0 then
				local t=(-b-sqrt(d))/a/2
				make_part(marker_cls,x-t*sa,y-t*ca,1)
			end
		end
	end

	-- create states
	states={
		drive=function()
			local ttl=0
			sprite,z,dz,nuke_mode=default_sprite,1,0
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
				local xarea,yarea=get_area(self,dx,0),get_area(self,0,dy)
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
		x=x,
		y=y,
		w=0.4,
		h=0.4,
		-- sprite width
		sw=16,
		sh=16,
		-- screen pos
		sx=54,
		sy=106,
		get_pos=function()
			return x,y,z,angle
		end,
		draw=function(self)
			-- player
			spr(sprite.spr,self.sx,self.sy,2,2)

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
			-- export current hitmask
			self.hitmask=sprite.hitmask

			-- "export" public variables
			self.x=x
			self.y=y
			-- kill "ghost" rotation
			if(abs(da)<0.001) da=0
		end
	}
end
local plyr=make_plyr(41,54,2,0)

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

-- particles: do not interact with actors or background
-- bullets: interact with actors or background + can spawn particles
local _parts,_bullets={},{}

function update_parts(parts)
	local px,py,pz,pangle=plyr:get_pos()
	local ca,sa=cos(pangle),sin(pangle)

	for i,p in pairs(parts) do
		p.t+=1
		-- elapsed?
		-- todo: fix
		if(p.ttl and p.t>=p.ttl) parts[i]=nil break

		-- custom update?
		if p.update then
			parts[i]=p:update()
		-- standard bullet			
		elseif p.kind==0 then
			local x0,y0=p.x,p.y
			local x1,y1=x0+p.dx,y0+p.dy
				
			-- hit player?
			if p.side==1 and pz<=1 then
				local x,y=x1-px,y1-py
				-- screen position
				p.sx=64+shl(ca*x-sa*y,3)-p.sw/2
				p.sy=112+shl(sa*x+ca*y,3)-p.sh/2

				local col,a,b,x0,y0,y1=collide(p,plyr)
				if col and intersect_bitmasks(a,b,x0,y0,y1) then
					local ax,ay=flr(a.sx),flr(a.sy)
					p.rect={ax+x0,ay+y0,ax+a.sw-1,ay+y1-1}
					--assert(false,"hit")
					--parts[i]=nil
					--goto die
				end
			else
				-- todo:
			end

			-- hit wall?
			if solid(x0,y1) or solid(x1,y0) then
				parts[i]=nil
				-- effect
				make_part({sw=2,c=7,ttl=2+rnd(2),kind=5},x1,y1,1)
			else
				p.x=x1
				p.y=y1
			end
::die::
		-- nuke
		elseif p.kind==1 then
			p.x+=p.dx
			p.y+=p.dy
			p.z+=p.dz
			if p.z<=1 then
				do_async(function() make_nuke(p.x,p.y) end)
				parts[i]=nil
			end
			-- gravity
			p.dz+=gravity
		end
	end
end

function draw_parts(parts,x0,y0,z0,angle)
	local ca,sa=cos(angle),sin(angle)
	for _,p in pairs(parts) do
		-- actual "depth"
		local z=p.z-z0+1
		-- todo: front of cam?
		if true then --z>0.1 then
			local x,y,w,h=(p.x-x0)/z0,(p.y-y0)/z0,p.sw and max(p.sw*z) or 0,p.sh and max(p.sh*z) or 0
			-- project
			local dx,dy=64+shl(ca*x-sa*y,3),112+shl(sa*x+ca*y,3)
			if p.draw then
				p:draw(dx-w/2,dy-h/2,z0)
			elseif p.kind==3 then
				local sx,sy,w=48,47,17
				if(time_t%8<4) sx,sy,w=61,32,13
				sspr(sx,sy,w,w,dx-w/2,dy-w/2)
			elseif p.kind==5 then
				-- bullet impact
				circfill(dx,dy,w,p.c)
			else
				sspr(p.ssx,p.ssy,p.sw,p.sh,dx-w/2,dy-h/2,w,h)
				-- debug
				if p.rect then
					local r=p.rect
					rect(r[1],r[2],r[3],r[4],8)
					p.rect=nil
				end
			end
		end
	end
end

function make_part(base_cls,x,y,z,dx,dy,dz)
	return add(_parts,setmetatable({
		-- age
		t=0,
		x=x,
		y=y,
		z=z or 1,
		dx=dx or 0,
		dy=dy or 0,
		dz=dz or 0
	},
	{__index=base_cls}))
end

function make_bullet(base_cls,x,y,z,dx,dy,dz)
	return add(_bullets,setmetatable({
		-- age
		t=0,
		x=x,
		y=y,
		z=z or 1,
		dx=dx or 0,
		dy=dy or 0,
		dz=dz or 0
	},
	{__index=base_cls}))
end

-- enemy bullets
local small_bullet_cls=with_hitmask({
	ssx=16,ssy=40,
	sw=8,sh=8,
	kind=0,
	side=1
})
local large_bullet_cls=with_hitmask({
	ssx=16,ssy=32,
	sw=8,sh=8,
	kind=0,
	side=1
})

-- blast

local blasts={}
local blast_circles={
	{{r=8,c=7}},
	{{r=6,c=0}},
	{{r=5,c=2},{r=4,c=9},{r=3,c=10},{r=2,c=7}},
	{{r=7,c=2},{r=6,c=9},{r=5,c=10},{r=4,c=7}},
	{{r=8,c=0},{r=6,c=2},{r=5,c=9},{r=3,c=10},{r=1,c=7}},
	{{r=8,c=0,fp=0xa5a5.ff,fn=circ}},
	{{r=8,c=0,fp=0x5a5a.ff,fn=circ}},
	{{r=8,c=0,fp=0x5a5a.ff,fn=circ}}
   }
--local blast_spr=make_memspr(80,32,16,16,nil,14)
local blast_cls={
	draw=function(self,x,y)
		palt(0,false)
		palt(14,true)
		local cc=lerpa(blast_circles,self.t/self.ttl)
		for i=1,#cc do
			local c=cc[i]
			if(c.fp) fillp(c.fp)
			(c.fn or circfill)(x,y,c.r,c.c)
			fillp()
		end
		palt()
	end,
	update=function(self)
		-- blast crater
		if(self.t==5) add(blasts,{x=x,y=y})
		return self
	end
}
function make_blast(x,y)
	-- explosion part(s)
	return make_part(blast_cls,x,y,1)
end

-- nuke "particle"
function make_nuke(x,y)
	local r0,r1,fp,t=8,8,#dither_pat,0
	-- nuke always at floor level
	return make_part({
		update=function(self)
			t+=1
			-- todo: blast npc in range!
			if(t>27) return
			return self
		end,
		draw=function(self,x,y,z)
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
	},x,y,1)
end

-->8
-- lzw map unpacking functions
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

-- lzw decompression from cart rom
-- reference: https://github.com/coells/100days
function decompress(mem,fn)
	local code,code_len,code_bits={},256,8
	for i=0,255 do
		code[shr(i,16)]={i}
	end
    local buffer,buffer_bits,index,prefix=0,0,0

	local x,y,len=0,0,peek2(mem)
	mem+=2
    while index < len or buffer_bits >= code_bits do
        -- read buffer
		while index < len and buffer_bits < code_bits do
			buffer=bor(shl(buffer,8),shr(peek(mem),16))
        	buffer_bits+=8
			index+=1
			mem+=1
		end
        -- find word
        buffer_bits-=code_bits
        local key=lshr(buffer,buffer_bits)
		buffer=band(buffer,shl(0x0.0001,buffer_bits)-0x0.0001)
		local word=code[key]
		if(not word) word=array_add(prefix, prefix[1])

        -- store word
		if prefix then
			code[shr(code_len,16)]=array_add(prefix, word[1])
			code_len+=1
		end
		prefix = word

        -- code length
		if code_len >= 2^code_bits then
			code_bits += 1
		end

		for i=1,#word do
			local s=word[i]
			-- 0: empty tile
			fn(s,x,y)
			x+=1
			if(x>127) x=0 y+=1 
		end
	end
	return mem
end

-->8
-- npc functions
local npcs={}

function pop(a)
	if #a>0 then
		local p=a[#a]
		a[#a]=nil
		return p
	end
end

-- a-star
local a_sides={{1,0},{0,1},{-1,0},{0,-1}}
function closest(x,y,nodes)
	local score,node=32000
	for _,v in pairs(nodes) do
		local vscore=dist(v.x,v.y,x,y)
		if vscore<score then
			node,score=v,vscore
		end
	end
	return node
end
function update_path_async(self)
::seek::
	while self.hp>0 do
		local x1,y1=plyr.x,plyr.y
		if self.flee then
			-- todo: review
			local pr,cr=whereami(plyr),whereami(self)
			local r=rooms[flr(16*pr+8*cr+self.id)%#rooms+1]
			x1,y1=rndlerp(r.x,r.x+r.w),rndlerp(r.y,r.y+r.h)
		else
			-- avoid all actors moving to player at once!
			if dist(x1,y1,self.x,self.y)>96 then
				yield()
				goto seek
			end
		end
	
	 	local x,y=self.x,self.y
		local k,pk=flr(x)+128*flr(y),flr(x1)+128*flr(y1)
		local flood,flood_len={[k]={x=x,y=y,k=k}},1
		local closedset,camefrom,current={},{}

		-- a* (+keep cpu/memory limits)
		while flood_len>0 and flood_len<24 do
			current=closest(x1,y1,flood)
			
			x,y,k=current.x,current.y,current.k
			if (k==pk) break
			flood[k],closedset[k]=nil,true
			flood_len-=1
	
			for _,d in pairs(a_sides) do
				local nx,ny=x,y
				-- works only for quadrants
				-- not a wall?
				if band(get_area({x=nx,y=ny,w=self.w,h=self.h},d[1],d[2]),0x1)==0 then
					nx+=d[1]
					ny+=d[2]
				end
				k=flr(nx)+128*flr(ny)
				if not closedset[k] and not camefrom[k] then
					flood[k],camefrom[k]={x=nx,y=ny,k=k},current
					flood_len+=1
				end
			end
		end
	
		local path,prev={},current
		while current do
			add(path,current)
			prev,current=current,camefrom[current.k]
		end
		self.path=path

		-- wait path completion or timeout
		local t=time_t+self.seek_dly
		while #self.path>0 do
			if(t<time_t) break
			yield()
		end
		self.input=nil
	end
end

-- npc 
function make_npc(base,x,y)
	local angle,hit_t=0,0
	-- quad
	-- texspace -> world space
	local w,h=0.5*base.sw/8,0.5*base.sh/8
	local quad={
		{x=0,y=0,ix=-h,iy=w},
		{x=0,y=0,ix=h,iy=w},
		{x=0,y=0,ix=h,iy=-w},
		{x=0,y=0,ix=-h,iy=-w}
	}
	
	return setmetatable({
		-- coords in world units
		x=x,
		y=y,
		draw=function(self,x0,y0,z0,a0)
			local ca,sa=cos(a0),-sin(a0)
			local x1,y1=self.x-x0,self.y-y0
			local scale=1/z0
			-- position in screen space (map units)
			x1,y1=scale*(ca*x1+sa*y1)+8,scale*(-sa*x1+ca*y1)+14
			
			ca,sa=cos(angle-a0),-sin(angle-a0)
			local outcode=0xffff
			for _,g in pairs(quad) do
				-- rotate in local space
				local ix,iy=g.ix,g.iy
				-- translate to cam space
				ix,iy=scale*(ca*ix-sa*iy)+x1,scale*(sa*ix+ca*iy)+y1
				local code=0
				if ix>16 then code=2
				elseif ix<0 then code=1 end
				if iy>16 then code+=8
				elseif iy<0 then code+=4 end
				outcode=band(outcode,code)
				-- to screen space
				g.x=8*ix
				g.y=8*iy
			end
			-- visible?
			if outcode==0 then
				polytex(quad,base.uv)

				print(angle,8*x1,8*y1,11)
			end
			if self.input then
				local ca,sa=cos(angle),sin(angle)
				local x0,y0=8*self.x,8*self.y
				local x1,y1=8*self.input.x,8*self.input.y
				line(x0,y0,x1,y1,11)
				x1,y1=x0+16*ca,y0+16*sa
				line(x0,y0,x1,y1,8)
			end
		end,
		die=function(self)
			make_blast(x,y)
		end,
		hit=function(self,dmg)
			hp-=dmg
			hit_t=4
			if hp<=0 then
				-- 
				self:die()
				return
			end
		end,
		update=function(self)
			hit_t-=1
			angle=self:control()

			-- todo: provide as parameters?
			local px,py,pz,pangle=plyr:get_pos() 

			--angle=atan2(x-px,-y+py)

			-- todo: materialize coords: 8*
			--self.x=x
			--self.y=y
		end
	},
	-- merge with base class
	{__index=base})
end
-- create actors
local npc_id=0
function make_tank(x,y)
	local angle,acc,move_t=0,0.1,0
	local update_path=cocreate(update_path_async)
	local dx,dy=0,0
	-- can npc move?
	local id=npc_id
	npc_id+=1

	local light_tank_cls={
		w=0.8,
		h=0.8,
		sh=16,
		sw=16,
		uv={
			0,0,
			2,0,
			2,2,
			0,2},
		hp=1,
		-- co-routine data
		seek_dly=60,
		path={},
		control=function(self)
			move_t-=1
			if move_t<0 and #self.path>0 then
				-- get result from a*
				local input=self.input
				if not input or dist(self.x,self.y,input.x,input.y)<1 then
					input=pop(self.path)
					self.input=input
				end
				if input then
					local target_angle=atan2(input.x-self.x,input.y-self.y)
					-- shortest angle
					local dtheta=target_angle-angle
					if dtheta>0.5 then
						angle+=1
					elseif dtheta<-0.5 then
						angle-=1
					end
					angle=lerp(angle,target_angle,0.1)
					
					local ca,sa=cos(angle),sin(angle)
					dx=0.1*ca
					dy=0.1*sa
					
					--[[
					local u,v=normalize(input.x-self.x,input.y-self.y,0.8*base.acc)
					dx=u
					dy=v
					]]
				end
			end
	
			-- update pos
			local xarea,yarea=get_area(self,dx,0),get_area(self,0,dy)
			-- solid?
			if band(xarea,0x1)==0 then
				--self.x+=dx
			end
			if band(yarea,0x1)==0 then
				--self.y+=dy
			end
			dx*=0.9
			dy*=0.9
	
			-- compute path for only 1 actor/frame
			if id==(time_t%npc_id) then
				assert(coresume(update_path,self))
			end

			return angle
		end
	}

	return make_npc(light_tank_cls,x,y)
end

function make_heavy_turret(x,y)
	local angle,reload_ttl=0,0
	local heavy_turret_cls={
		sh=32,
		sw=32,
		uv={
			2,0,
			6,0,
			6,4,
			2,4},
		hp=10,
		control=function(self)
			local x,y=self.x,self.y
			-- atan2 is fast enough
			local target_angle=atan2(plyr.x-x,-plyr.y+y)
			-- shortest angle
			local dtheta=target_angle-angle
			if dtheta>0.5 then
				angle+=1
			elseif dtheta<-0.5 then
				angle-=1
			end
			angle=lerp(angle,target_angle,0.1)

			-- close enough?
			reload_ttl-=1
			if reload_ttl<0 and dist(x,y,plyr.x,plyr.y)<20 then
				reload_ttl=40
				local ca,sa=cos(angle),-sin(angle)
				-- center position
				local w,cx,cy=1.5/3,x+2*ca,y+2*sa
				for i=-1,1 do
					do_async(function()
						wait_async((i+1)*10)
						-- still valid?
						if self.hp>0 then
							make_bullet(large_bullet_cls,cx-i*sa*w,cy+i*ca*w,1,0.12*ca,0.12*sa)				
						end
					end)
				end
			end
			-- accomate sprite orientation
			return angle+0.25
		end
	}
	return make_npc(heavy_turret_cls,x,y)
end
	
-->8
-- map helpers
function polytex(v,uv)
	local p0,nodes=v[#v],{}
	local x0,y0,u0,v0=p0.x,p0.y,uv[7],uv[8]
	for i=1,#v do
		local p1=v[i]
		local x1,y1,u1,v1=p1.x,p1.y,uv[i*2-1],uv[i*2]
		local _x1,_y1,_u1,_v1=x1,y1,u1,v1
		if(y0>y1) x0,y0,x1,y1,u0,v0,u1,v1=x1,y1,x0,y0,u1,v1,u0,v0
		local dy=y1-y0
		local dx,du,dv=(x1-x0)/dy,(u1-u0)/dy,(v1-v0)/dy
		if(y0<0) x0-=y0*dx u0-=y0*du v0-=y0*dv y0=0
		local cy0=ceil(y0)
		-- sub-pix shift
		local sy=cy0-y0
		x0+=sy*dx
		u0+=sy*du
		v0+=sy*dv
		for y=cy0,min(ceil(y1)-1,127) do
			local x=nodes[y]
			if x then
				--rectfill(x[1],y,x0,y,offset/16)
				
				local a,au,av,b,bu,bv=x.x,x.u,x.v,x0,u0,v0
				if(a>b) a,au,av,b,bu,bv=b,bu,bv,a,au,av
				local dab=b-a
				local dau,dav=(bu-au)/dab,(bv-av)/dab
				local ca=ceil(a)
				-- sub-pix shift
				local sa=ca-a
				au+=sa*dau
				av+=sa*dav
				tline(ca,y,ceil(b)-1,y,au,av,dau,dav)
			else
				nodes[y]={x=x0,u=u0,v=v0}
			end
			x0+=dx
			u0+=du
			v0+=dv
		end
		x0,y0,u0,v0=_x1,_y1,_u1,_v1
	end

	--[[
	local v0,v1,v2,v3=
		v[1],
		v[2],
		v[3],
		v[4]
	line(v0.x,v0.y,v1.x,v1.y,7)
	line(v1.x,v1.y,v2.x,v2.y,7)
	line(v2.x,v2.y,v3.x,v3.y,7)
	line(v3.x,v3.y,v0.x,v0.y,7)
	]]
end

function draw_map(x,y,z,a)
	local ca,sa=cos(a),-sin(a)
	local scale=1/z
	-- project all potential tiles
	for i,g in pairs(_grid) do
		-- to cam space
		local ix,iy=32*(i%5)-x,32*flr(i/5)-y
		ix,iy=scale*(ca*ix+sa*iy)+8,scale*(-sa*ix+ca*iy)+14
		local outcode=0
		if ix>16 then outcode=2
		elseif ix<0 then outcode=1 end
		if iy>16 then outcode+=8
		elseif iy<0 then outcode+=4 end
		-- to screen space
		g.x=8*ix
		g.y=8*iy
		g.outcode=outcode
	end
	
	-- collect visible cells
	local viz={}
	for k,cell in pairs(_cells) do
		local out=band(
			band(cell[1].outcode,
	   		band(cell[2].outcode,cell[3].outcode)),
	   		cell[4].outcode)
		-- visible or partially visible?
		if(out==0) viz[k]=cell
	end
	
	-- draw existing cache entries
	for i,entry in pairs(_map_lru) do
		local cell=viz[entry.k]
		if cell then
			local offset=32*i
			polytex(cell,{
				offset,0,
				32+offset,0,
				32+offset,32,
				offset,32})
			-- update lru time
			entry.t=time_t
			-- done
			viz[entry.k]=nil
		end
	end
   
	-- remaining tiles
	-- (e.g. cache misses)
	for k,cell in pairs(viz) do
		local mint,mini=32000,#_map_lru+1
		-- cache full?
		if mini>3 then
			-- find lru entry
			for i=1,3 do
				local entry=_map_lru[i]
				if entry.t<mint then
					mint,mini=entry.t,i
				end
			end
		end
		-- add/reuse cache entry
		_map_lru[mini]={k=k,t=time_t}
		-- fill cache entry
		local mem=0x2000+mini*32
		for base,v in pairs(_cells_map[k]) do
			poke4(mem+base,v)
		end
		-- draw with fresh cache entry		
		local offset=32*mini
		polytex(cell,{
			offset,0,
			32+offset,0,
			32+offset,32,
			offset,32})
	end  
	--[[
	local y=12
	for i=1,#_map_lru do
		local entry=_map_lru[i]
		print(entry.k..":"..entry.t,2,y,entry.t==time_t and 11 or 2)
		y+=6
	end
	for k,_ in pairs(viz) do
		print(k,2,y,8)
		y+=6
	end
	]]
end

-->8
-- init/update/draw
function _init()
	-- collision map
	_map,_cells,_cells_map,_grid,_map_lru={},{},{},{},{}
	-- 
	local grid_w=4
	-- grid_w intervals = grid_w+1^2 points
	for i=0,(grid_w+1)*(grid_w+1)-1 do
		_grid[i]={x=0,y=0,outcode=0}
	end
	-- direct link to grid vertices?
	for i=0,grid_w*grid_w-1 do
		-- cell coords in grid space 
		-- 5: account for 'additional' 5th point
		local ci=(i%grid_w)+(grid_w+1)*flr(i/grid_w)
		local tiles={
			_grid[ci],
			_grid[ci+1],
			_grid[ci+grid_w+2],
			_grid[ci+grid_w+1]
		}
		_cells[i]=tiles
		_cells_map[i]={}
	end

	local mem=decompress(0x2000,function(s,i,j)
		-- if(s==0) s=flr(24+rnd(3))
		-- no need to record static tiles
		if(s!=0) _map[i+128*j]=s

		if fget(s,2) then
			jumppads[i+128*j]=3
		end

		-- cell coord (128x128)->(4x4)
		local ci,cj=flr(i/32),flr(j/32)
		local ck=bor(ci,shl(cj,2))
		local cell_map=_cells_map[ck]
		assert(cell_map,ci.."/"..cj..":"..ck)
		-- cell is 4*dword with a stride of 128/4 = 32
		-- cell entry is packed as a dword
		local k=4*(flr(band(i,31)/4)+shl(band(j,31),5))
		local m=cell_map[k] or 0
		-- shift 
		m=bor(m,shl(0x0.0001,8*band(i,3))*s)
		cell_map[k]=m
		-- dword-packed map
		_cells_map[ck]=cell_map

		--rectfill(ci*16,cj*16,(ci+1)*16-1,(cj+1)*16-1,1)
		--print(ck.."\n"..c,ci*16+1,cj*16+1,2)
	end)
	-- decompress actors
	-- avoid overwriting ram while reading..
	local tmp={}
	decompress(mem,function(s,i,j)
		if(s!=0) add(tmp,{i=i,j=j,s=s})
	end)
	for _,t in pairs(tmp) do
		mset(t.i,t.j,t.s)
	end
	
	add(npcs,make_tank(33,60))
	add(npcs,make_heavy_turret(23,35))
	add(npcs,make_heavy_turret(30,35))
end

function _update()
	-- any futures?
	for k,f in pairs(_futures) do
		local cs=costatus(f)
		if cs=="suspended" then
			assert(coresume(f))
		elseif cs=="dead" then
			_futures[k]=nil
		end
	end

	cam_update()

	plyr:update()
	for i=1,#npcs do
		npcs[i]:update()
	end

	update_parts(_bullets)
	update_parts(_parts)

	time_t+=1
end

local red_blink={0,1,2,2,8,8,8,2,2,1}

function _draw()
	local px,py,pz,pangle=plyr:get_pos()

	cls()
	draw_map(px,py,pz,pangle)

	-- commit blasts to texmap
	for _,b in pairs(blasts) do
		blast_spr:draw(_texmap,8*b.x,8*b.y)
	end
	blasts={}

	-- draw
	for i=1,#npcs do
		npcs[i]:draw(px,py,pz,pangle)
	end

	plyr:draw()

	draw_parts(_bullets,px,py,pz,pangle)
	draw_parts(_parts,px,py,pz,pangle)

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
0000111111110000022ee22033333357277777727d33333300000000000000000008000000000000eeeee08500eeeeee00000000000000000000000000000000
000282222228200002e77e20333335d82666666827d3333300000000000000000000000000000000ee0ee052800ee0ee00000000000000000000000000000000
000442499424400002e77e20333335eee666666e82d3333377000000000778000008000008000000eeee042014000eee00000000000000000000000000000000
0005549229455000022ee22033335d6666666666667d333370000000000078800080800088000000eee04d60010040ee00000000000000000000000000000000
000d549dd945d0000022220033335d6666666666667d333370000070000078880800080888000000e0e085d8024da0ee000000000000005d7600000000000000
000559f99f955000000000003335d666666226666667d33370000000000078800080800088000000ee0880820285d00e00000000000055d66776000000000000
000d59faaf95d000000000003335d666662882666667d33377000000000778000008000008000000ee04d000002800ee0000000000055d666777600000000000
000556ffff65500000000000335d66666288882666667d3300000000000000000000000000000000e005a808d002821e00000000007777777777770000000000
000d45f76f54d00000099000335d66662888888266667d3300000000000000000008000000000000ee014005940001ee000000000d6666666666667000000000
0009d425124d9000009a79003126666e88888888266662d300007000700000000088800000000000eeee100250280eee000000000d6666666666667000000000
0000a907609a0000019aa9003182666e88888888266628d300007777700000000888880000000000eeeee0080082eeee000000005dd666666666667600000000
00000007d000000001199000318e6666e88888826666e8d300000000000000000000000000000000ee0ee100000e0eee00000005ddd666666666667760000000
00000000000000000011000031e66666e888888266666ed300000000000000000000000000000000eeeeee01e0eeeeee0000000dddd666666666667770000000
000000000000000000000000331d66666e8888266666d53300000088888000000000000000000000eeeeeeeeeeeeeeee000000dd5dd666666666676777000000
0000000000000000000000003331d6666eeeeee6666d5333000000088800000000000000336667766776677667766633000000555dd666666666676666000000
00000000000000000000000033331d666666666666d533330000000080000000000000003676d776d776d776d7766763000000155dd66666666667666d000000
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
00049f1491f9400001cc7c00ddddddddddd77dddd7d7dddd00000000000000000000000066ddddddddddddddddddd16600000000001670167016700000000000
0001481111841000011cc000dddddddddd7dd7ddd77ddddd000000008000000000000000677dddddddddddddddddd77600000000001670167016700000000000
000011000011000000110000ddddddddd777777dd7dddddd0000000888000000000000001d76677667766776677667d100000000000000000000000000000000
000000000000000000000000dddddddddddddddddddddddd0000008888800000000000001156d776d776d776d776d51100000000000000000000000000000000
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
d70400804060503824160d0784426150b864361d0f8844625138a4562d178c466351b8e4763d1f904864523924964d27944a6552b964b65d2f984c665339a4d66d379c4e656d09e4f67d3f9f4ea773c0100e8d47a45268e046850a4ed0a2d1982acaa556ad57aa2a8294da7495a012aa806c40109d96cd67b3d6ab95d91d7eca
01475c51f64b28140d77bc0180e13b55b2496eb827b049fba04c6e38c46202c160bdf2b77eb684827704723f2b85c3e24718bc6df72121c02393c9fd1e63359bc663ad79f8f6872a8ec284c4a26da6d44c27d56b2416ec9e96d1bfb2ac04623126af751bde64f63c0b356e79c78fb40442358d8fadd6e170f87c5e8682bf76bc
de40e129ff77221519fa7d5ea0bf93cd26af85717f3fa7b78defdd84be5f4fafbbf1e77f1fd7ddff475f1805f37da0448a0681d8c04a0a82c120300d0380f62c3684e157fa106b4120701d0781f62c3487e2186e1c4715f0b02d0b82f88e2b8b6278a11a57c3c0f43e0fe238da388ca334615f0d2410d623904340d63d8f9168
320782510501cf9240057db36d9b609e4842150528035324f80dc757dd775caa9610596953560c096e5c97dac985cc006649b50457cad59810045662ad58550c0531f86f2789de799c90d72413080215a1712396963def6f28959a92a150ca1d660049f2389fa709ea6d669fa735b2975a0aba854041aa40048f27c8f23da3a6
d825959e74150305cc59e6a52a7f40dbc10c441156627aafa7aafab89f596a1ad402adeb89ea7ca9d3f6481314c54155685c1a4b149e5d2ca6eaa4b39c0ae949b346319065b85be6e6dfb4d65a0aee9e5930c432bd2f30c43104e7b55e76b62e158ec9af19fa1e9259692006f7bd3090caf7b39d7705dac40b15beb39954208b
12c370abdb08c36ff04cb071540bb714a893974b189c2f7be2f3c2ef4ae1d6594a98f697a56dfc5f0d0c56204f2cbce70c3ab440734c554eb81c05930bcf746cfe8f40b42c965e5052ecdee15bf0abf974d0100d390996a6b9b12ed15bf5931c730290a985d6652c8ef9b7a66b327c5576d4aaccd5313c1ef8730390eb3abaf4
dbb585dcb41dd2e1e05f0daf57d51b1da5bcdf17c92383d52a74a790d5f8dd2813d9ab3d3341b4f1ee2f87b3a6a97650847755bf1ecf813deb9a7df8cdf2a6c075ac8f9959fb4594acb8e5d48b94c77a8e9b9fe9f8ddcb94ead67f16cf9f66cc9513d8762c3ba6df79ca63969fbd2f41c09efa2f2d11f356665e62e99c209266
dffbe79144b37a6e5a8ef6e4ee8168a729af7ec3b72b8765dcf9167ceb8051fe9dd7be7d86b5a9ab8344609572ae3465ccdfbf774899a019ca75e5517e3ff80100601328380ab2041715d45a0b53db5545c20529d53872d6c40076cda51a1d3387061613f183a598ec9c421caa94d29e536a6d4e2ac7d6f4ce5bc78547209e42
c85cb859013d22103cdec07306ac156ac384c701db3ac24a06a21c4539911de644a00260e0e405349095babc78a853ce9c5863e769fc45b3a8b60c146f7e3055e8441417112191c46a292a33bfa4c472e199da8d059a3a16d1a0050e0c6b453129674475a4b864192485922102c2c621255884923a525a4b492255122423508f
2af65049e4a3296534a79512a6554ab9592b6574af9612c6594b39692d65b4b79712e65d4bb9792f65f4bf981306614c3989316634c7991326654cb999336674cf9a1346694d39a93566b4d79b13666d4db9b93766f4df26a0e60040209309a4e278020d0784426150b864361d0f88446251383140a25c2e978bf148e4763d1f
90444d86d371be4327944a655083e1f4fc7f95cc665339a4d66d379c4e6753b9e4f67d3fa05068543a25168d47a4526954ba65369d4fa8546a553aa556ad57ac566b55bae576bd5fb0586c563b2596cd67b45a6d56bb65b6dd6fb85c6e573ba5d6ed77bc5e6f57bbe5f6fd7fc06070583c26170d87c4627158bc66371d8fc864
72593ca6572d97cc667359bce6773d9fd068745a3d26974da7d46a755abd66b75dafd86c765b3da6d76db7dc6e775bbde6f77dbfe070785c3e27178dc7e4726980000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
