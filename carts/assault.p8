pico-8 cartridge // http://www.pico-8.com
version 23
__lua__
-- assault
-- by @freds72

#include includes/bold.lua
#include includes/bigscore.lua

-- misc helper functions
local shkx,shky=0,0
function cam_shake()
	shkx,shky=min(1,shkx+rnd()),min(1,shky+rnd())
end
function cam_update()
	shkx*=-0.7-rnd(0.2)
	shky*=-0.7-rnd(0.2)
	if abs(shkx)<0.5 and abs(shky)<0.5 then
		shkx,shky=0,0
	end
	camera(shkx,shky)
end

function lerp(a,b,t)
	return a*(1-t)+t*b
end

-- pick a value from an array
-- t must be [0,1[
function lerpa(a,t)
	return a[flr(#a*t)+1]
end
-- pick the next item in a table based on time
function picknext(a,spd)
	return a[flr(30*time()*spd)%#a+1]
end

-- return shortest angle to target
function make_lerp_angle(angle,pow)
	return function(x0,y0,x1,y1)
		local target_angle=atan2(x1-x0,-y1+y0)
		-- shortest angle
		local dtheta=target_angle-angle
		if dtheta>0.5 then
			angle+=1
		elseif dtheta<-0.5 then
			angle-=1
		end
		angle=lerp(angle,target_angle,pow)
		return angle
	end
end

function make_v(a,b)
	return {x=b.x-a.x,y=b.y-a.y}
end

-- manhattan distance (safe for overflow)
function dist(x0,y0,x1,y1)
	return abs(x1-x0)+abs(y1-y0)
end

function padding(s)
	return sub("00000",1,5-#s)..s
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
		if(fn) fn(i)
		yield()
	end
end

-->8
-- sprite helpers
-- convert a sprite number to a sprite stucture
function make_sprite(s,sw,sh)
	sw=sw or 8
	sh=sh or 8
	return {spr=s,ssx=band(s<<3,127),ssy=8*(s\16),sw=sw,sh=sh,sx=-sw/2,sy=-sh/2}
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
			if(sget(sx+i,sy+j)!=tc) bits|=0x8000>>i
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
	local ah,bh=ay+a.sh,by+b.sh
	if ax<bx+b.sw and 
		ax+a.sw>bx and
		ay<bh and
		ah>by then
	 	-- collision coords in 'a' space
 		return true,a,b,bx-ax,max(by-ay),min(bh,ah)-ay
	end
end

-->8
-- game globals
local time_t=0
local jumppads={}
-- turrets: stores number of heavy turrets
local _npcs,plyr,_turrets,_blasts,_npc_map
local _map,_cells,_cells_map,_grid,_map_lru

-- todo: remove for optimisation
local gravity=-0.04
local red_blink={0,1,2,2,8,8,8,2,2,1}

-- actors mapr
function to_npc_map(x,y)
	return x\2|(y\2)<<6
end

-->8
-- player
function make_plyr(x,y,z,angle)
	local reload_ttl,reload_nuke_ttl,acc,da,dz,mortar_angle,underwater=0,0,0,0,0,0
	-- threads positions & velocities
	local lthread,rthread,lthread_acc,rthread_acc=0,0,0,0
	local states,state,nuke_mode,driving_mode,dead_mode
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
			default_sprite,
			with_hitmask(make_sprite(134,16,16)),
			with_hitmask(make_sprite(136,16,16))
		}

	-- select default
	local sprite,dust_sprites=default_sprite,{212,213,214}

	-- nuke velocity
	-- todo: remove
	local nuke_v=0.7
	
	-- player-specific particles
	-- regular bullet
	local bullet_cls=with_hitmask({
		ttl=30,
		ssx=16,ssy=56,
		sw=8,sh=8,
		kind=0,
		side=2
	})
	local nuke_shell_cls={
		ttl=90,
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
			make_bullet(bullet_cls,x,y,0,-sa/2,-ca/2)
			reload_ttl=10
			sfx(0)
		end
	end

	function fire_nuke(ttl)
		if btn(5) and reload_nuke_ttl<0 then
			-- polar coords
			local cm,sm=nuke_v*cos(mortar_angle),-nuke_v*sin(mortar_angle)
			local ca,sa=cm*cos(angle),cm*sin(angle)
			make_part(nuke_shell_cls,x,y,z,-sa,-ca,sm)
			-- next nuke
			reload_nuke_ttl=ttl
			sfx(4)
			-- marker
			local a,b,c=gravity/2,sm,z
			local d=b*b-4*a*c
			if d>=0 then
				local t=(-b-sqrt(d))/a/2
				make_part(marker_cls,x-t*sa,y-t*ca)
			end
		end
	end

	-- create states
	states={
		drive=function()
			local ttl=0
			sprite,z,dz,driving_mode,nuke_mode,dead_mode=default_sprite,0,0,true
		
			return function(self)
				-- flip?
				if btn(4) then
					if(btn(0)) state=states.flip(-1) return
					if(btn(1)) state=states.flip(1) return
					if(btn(3) and not underwater) state=states.mortar() return
				end

				-- reduce turn rate if driving while turning
				local turn_scale=1
				-- regular drive
				if(btn(2)) acc+=0.04 turn_scale=0.7 lthread_acc=1 rthread_acc=1
				if(btn(3)) acc-=0.04 turn_scale=0.7 lthread_acc=-1 rthread_acc=-1
				
				-- rotation
				if(btn(0)) da=-0.01 lthread_acc=-turn_scale rthread_acc=turn_scale
				if(btn(1)) da=0.01 lthread_acc=turn_scale rthread_acc=-turn_scale
				
				angle+=da*turn_scale
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
				local area=xarea|yarea
				underwater=nil
				if area==0x2 and abs(acc)>0.1 then
					cam_shake()
				-- jumppad
				elseif area==0x8 then
					underwater=true
				elseif band(area,0x4)>0 then
					local i=flr(x)|flr(y)<<7
					local j=jumppads[i]
					-- actvivate
					if j and j>0 then
						-- push
						dz=0.6
						state=states.airborne()
						-- 
						jumppads[i]-=1
					end
				end

				fire_bullet(ca,sa)

				-- friction
				da*=0.8
				acc*=0.7
				lthread_acc*=0.7
				if(abs(lthread_acc)<0.1) lthread_acc=0
				rthread_acc*=0.7
				if(abs(rthread_acc)<0.1) rthread_acc=0
			end
		end,
		flip=function(dir)
			local ttl,sprites=20,flip_sprites[dir]
			local ca,sa=cos(angle),sin(angle)
			local dx,dy=0.1*ca*dir,-0.1*sa*dir
			-- stop fwd & rotation
			acc,da,driving_mode=0,0
			return function(self)
				ttl-=1
				if(ttl<0) state=states.drive() return

				sprite=lerpa(sprites,ttl/20)
				-- get tile
				self.x,self.y=x,y
				-- solid?
				if band(get_area(self,dx,0),0x1)==0 then
					x+=dx
				end
				if band(get_area(self,0,dy,0),0x1)==0 then
					y+=dy
				end

				fire_bullet(ca,sa)
			end
		end,
		airborne=function()
			local ttl=0
			mortar_angle,nuke_mode,driving_mode=0,true
			return function()
				if(btn(0)) da=-0.01
				if(btn(1)) da=0.01
				
				angle+=da
				z+=dz
				dz+=gravity/5
				if z<0 then
					state=states.drive()
					return
				end
						
				fire_nuke(60)

				-- friction
				da*=0.9
				acc=0
			end
		end,
		mortar=function()
			nuke_mode=true
			-- stop fwd & rotation
			acc,da,mortar_angle,driving_mode=0,0,0.02
			return function()
				if(mortar_angle<0.02) state=states.drive() return

				-- fast exit from mortar mode
				if(btn(2)) state=states.drive() return
				if(btn(3)) mortar_angle+=0.01
				mortar_angle=mid(mortar_angle*0.95,0,0.2)				
				sprite=lerpa(mortar_sprites,mortar_angle/0.2)

				fire_nuke(90)
			end
		end,
		drop=function()
			local bounces=2
			driving_mode=nil
			return function()
				z+=dz
				dz+=gravity
				if z<0 then
					sfx(1)
					bounces-=1
					z=0
					if bounces>0 then
						dz=abs(dz)/4
					else
						state=states.drive()
					end
				end
			end
		end,
		die=function()
			local ttl=40
			acc,da=0,0
			dead_mode,driving_mode,nuke_mode,underwater=true
			return function(self)
				ttl-=1
				-- todo: wait for input + message + hide sprite
				if(ttl<0) make_part(small_blast_cls,x,y)
			end
		end
	}

	-- initial state
	state=states.drop()

	local bands={7,7,7,8,7,8,7,8}

	return {
		x=x,
		y=y,
		w=0.4,
		h=0.4,
		-- sprite width
		sw=16,
		sh=16,
		-- screen pos
		sx=56,
		sy=106,
		get_pos=function()
			return x,y,z,angle
		end,
		draw=function(self)
			if(dead_mode) memset(0x5f01,lerpa(bands,(4*time()%1)),15)
			if driving_mode then
				if(underwater) pal(5,6)
				if lthread_acc>0 then
					spr(picknext(dust_sprites,0.25),66,119,1,1,false,true)
				elseif lthread_acc<0 then
					spr(picknext(dust_sprites,0.25),66,104)
				end
				if rthread_acc>0 then
					spr(picknext(dust_sprites,0.25),54,119,1,1,true,true)
				elseif rthread_acc<0 then
					spr(picknext(dust_sprites,0.25),54,104,1,1,true)
				end
				if(underwater) pal()
			end
			-- player
			if(underwater) pal(1,3) pal(9,11) pal(4,11)
			spr(sprite.spr,56,106,2,2)
			
			if driving_mode then
				sspr(40+2*flr((lthread%3+3)%3),32,2,7,60,111)
				sspr(40+2*flr((rthread%3+3)%3),32,2,7,66,111,2,7,true)
				lthread+=lthread_acc
				rthread+=rthread_acc
			end
			pal()

			if nuke_mode then
				-- nuke estimated impact marker
				-- x=v*cos(mortar)*t
				-- y=0.5*g*t^2+v*sin(mortar)*t+y0
				local a,b,c=gravity/2,-nuke_v*sin(mortar_angle),z/8
				local d=b*b-4*a*c
				if d>=0 then
					local t=(-b-sqrt(d))/a/2
					local sy=nuke_v*cos(mortar_angle)*t
					local dx,dy=64,112-sy*8
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
		collide=function(self,p)
			if(dead_mode) return
			local col,a,b,x0,y0,y1=collide(self,p)
			if col and intersect_bitmasks(a,b,x0,y0,y1) then
				return true
			end
		end,
		hit=function()
			-- prent
			-- state=states.die()
		end,
		exit=function(self,path)
			state=states.find_exit(self,path)
		end,
		update=function(self)
			reload_ttl-=1
			reload_nuke_ttl-=1

			-- free up previous pos
			_npc_map[to_npc_map(self.x,self.y)]=nil
			
			state(self)
	
			-- export current hitmask
			self.hitmask=sprite.hitmask

			-- "export" public variables
			self.x=x
			self.y=y
	
		  --
		  _npc_map[to_npc_map(self.x,self.y)]=true

			-- kill "ghost" rotation
			if(abs(da)<0.001) da=0
		end
	}
end

-- particles: do not interact with actors or background
-- bullets: interact with actors or background + can spawn particles
local _parts,_bullets={},{}
-- blast data
local blast_circles={
	{{r=8,c=7}},
	{{r=6,c=0}},
	{{r=5,c=2},{r=4,c=8},{r=3,c=10},{r=2,c=7}},
	{{r=7,c=2},{r=6,c=8},{r=5,c=10},{r=4,c=7}},
	{{r=8,c=0},{r=6,c=2},{r=5,c=9},{r=3,c=10},{r=1,c=7}},
	{{r=8,c=0,fp=0xa5a5.ff,fn=circ}},
	{{r=8,c=0,fp=0x5a5a.ff,fn=circ}},
	{{r=8,c=0,fp=0x5a5a.ff,fn=circ}}
}

function update_parts(parts)
	local px,py,pz,pangle=plyr:get_pos()
	local ca,sa=cos(pangle),sin(pangle)

	for i,p in pairs(parts) do
		p.t+=1
		-- elapsed?
		if(p.t>=p.ttl) parts[i]=nil goto continue

		-- custom update?
		if p.update then
			parts[i]=p:update()
		-- standard bullet			
		elseif p.kind==0 then
			local x0,y0=p.x,p.y
			local x1,y1=x0+p.dx,y0+p.dy
				
			-- hit player on ground?
			if p.side==1 and pz<=0 then
				local x,y=x1-px,y1-py
				-- top/left corner screen position
				p.sx=64+((ca*x-sa*y)<<3)-p.sw/2
				p.sy=112+((sa*x+ca*y)<<3)-p.sh/2
				if plyr:collide(p) then
					plyr:hit()
					goto die
				end
			elseif p.side==2 then
				-- 
				for _,npc in pairs(_npcs) do
					if npc:collide(p) then
						npc:hit(1)
						goto die
					end
				end 
			end
		
			-- hit wall?
			if bullet_solid(x0,y1) or bullet_solid(x1,y0) then
				parts[i]=nil
				-- effect
				make_part({sw=2,c=7,ttl=2+rnd(2),kind=5},x0,y0,1)
			else
				p.x=x1
				p.y=y1
			end
			goto continue
::die::
			parts[i]=nil
		-- nuke
		elseif p.kind==1 then
			p.x+=p.dx
			p.y+=p.dy
			p.z+=p.dz
			-- gravity
			p.dz+=gravity
			if p.z<=0 then
				local x,y=p.x,p.y
				do_async(function() make_nuke(x,y) end)
				parts[i]=nil
			end
		end
::continue::
	end
end

function draw_parts(parts,x0,y0,z0,angle)
	local ca,sa=cos(angle),sin(angle)
	for _,p in pairs(parts) do
		-- actual "depth"
		local z,zw=8/(z0+8),(p.z+0.5)/(z0+0.5)
		-- todo: front of cam?
		if true then --z>0.1 then
			local x,y,w,h=(p.x-x0)*z,(p.y-y0)*z,p.sw and max(p.sw*zw) or 0,p.sh and max(p.sh*zw) or 0
			-- project
			local dx,dy=64+((ca*x-sa*y)<<3),112+((sa*x+ca*y)<<3)
			if p.draw then
				p:draw(dx-w/2,dy-h/2,z)
			elseif p.kind==3 then
				local sx,sy,w=48,47,17
				if(time_t%8<4) sx,sy,w=61,32,13
				sspr(sx,sy,w,w,dx-w/2,dy-w/2)
			elseif p.kind==5 then
				-- bullet impact
				circfill(dx,dy,0.5*w/p.t,p.c)
			elseif p.kind==7 then
				-- blast
				palt(0,false)
				palt(14,true)
				local cc,r=lerpa(blast_circles,p.t/p.ttl),p.r
				for _,c in ipairs(cc) do
					if(c.fp) fillp(c.fp)
					(c.fn or circfill)(dx,dy,r*c.r*zw,c.c)
					fillp()
				end
				palt()		
			else
				sspr(p.ssx,p.ssy,p.sw,p.sh,dx-w/2,dy-h/2,w,h)
			end
		end
	end
end

local _next_part=0
function make_part(base_cls,x,y,z,dx,dy,dz)
	local p=setmetatable({
		-- age
		t=0,
		x=x,
		y=y,
		z=z or 0,
		dx=dx or 0,
		dy=dy or 0,
		dz=dz or 0
	},{__index=base_cls})
	_parts[_next_part]=p
	_next_part=(_next_part+1)%1024
	if(p.sfx) sfx(p.sfx)
	return p
end

local _next_blt=0
function make_bullet(base_cls,x,y,z,dx,dy,dz)
	local b=setmetatable({
		-- age
		t=0,
		x=x,
		y=y,
		z=z or 0,
		dx=dx or 0,
		dy=dy or 0,
		dz=dz or 0
	},{__index=base_cls})
	_bullets[_next_blt]=b
	_next_blt=(_next_blt+1)%1024
	return b
end

-- enemy bullets
local small_bullet_cls=with_hitmask({
	ssx=16,ssy=40,
	sw=8,sh=8,
	kind=0,
	side=1,
	ttl=40
})
local large_bullet_cls=with_hitmask({
	ssx=16,ssy=32,
	sw=8,sh=8,
	kind=0,
	side=1,
	ttl=180
})

-- blast
local small_blast_cls={
	ttl=10,
	kind=7,
	r=1,
	sfx=2
}
local large_blast_cls={
	ttl=30,
	kind=7,
	r=3
}

-- nuke "particle"
function make_nuke(x,y)
	sfx(3)
	local r0,r1,fp,t=8,8,#dither_pat,0
	-- nuke always at floor level
	return make_part({
		ttl=27,
		update=function(self)
			if self.t==26 then
				-- find all npc in range
				for _,npc in pairs(_npcs) do
					if dist(x,y,npc.x,npc.y)<8 then
						npc:hit(10)
					end
				end 
			end
			return self
		end,
		draw=function(self,x,y,w)
			--
			local t=self.t
			if t<4 then
				fade(t)
			else
				pal()
				r0=lerp(r0,45,0.22)*w
				r1=lerp(r1,45,0.3)*w
				local rr0,rr1=r0*r0,r1*r1
				if(t>20) fp=lerp(fp,1,0.1)	
				fillp(dither_pat[flr(fp)]+0x0.ff)
				local cx,cy=camera(-x,-y)
				color(0x77)
				for r=0,r1 do
					local x0,x1=sqrt(rr0-r*r),sqrt(rr1-r*r)
					rectfill(-x0,r,-x1,r)
					rectfill(x0,r,x1,r)
					rectfill(-x0,-r,-x1,-r)
					rectfill(x0,-r,x1,-r)    
				end
				circ(0,0,r0,12)
				fillp()
				-- restore camera settings
				camera(cx,cy)
			end
		end
	},x,y)
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
		code[i>>16]={i}
	end
    local buffer,buffer_bits,index,prefix=0,0,0

	local x,y,len=0,0,peek2(mem)
	mem+=2
    while index < len or buffer_bits >= code_bits do
        -- read buffer
		while index < len and buffer_bits < code_bits do
			buffer=buffer<<8|@mem>>16
      buffer_bits+=8
			index+=1
			mem+=1
		end
    -- find word
    buffer_bits-=code_bits
    local key=buffer>>buffer_bits
		buffer&=(0x0.0001<<buffer_bits)-0x0.0001
		local word=code[key]
		if(not word) word=array_add(prefix, prefix[1])

        -- store word
		if prefix then
			code[code_len>>16]=array_add(prefix, word[1])
			code_len+=1
		end
		prefix = word

        -- code length
		if code_len >= (1<<code_bits) then
			code_bits += 1
		end

		for i=1,#word do
			-- 0: empty tile
			fn(word[i],x,y)
			x+=1
			if(x>127) x=0 y+=1 
		end
	end
	return mem
end

-->8
-- npc
function solid_npc(a,dx,dy)
	local x,y,w,h=a.x+dx,a.y+dy,a.w,a.h
	return 
		_npc_map[to_npc_map(x-w,y-h)] or 
		_npc_map[to_npc_map(x+w,y-h)] or 
		_npc_map[to_npc_map(x-w,y+h)] or 
		_npc_map[to_npc_map(x+w,y+h)]
end

function make_npc(base,x,y,angle,name)
	local acc,hit_t=0,0

	-- quad
	-- texspace -> world space
	-- note: useless for 'map' npc
	local s=base.sprite or base
	local w,h=s.sw>>4,s.sh>>4
	-- x---->x
	--    0  |
	-- x<----x
	local quad={
		{x=0,y=0,ix=-w,iy=-h},
		{x=0,y=0,ix=w,iy=-h},
		{x=0,y=0,ix=w,iy=h},
		{x=0,y=0,ix=-w,iy=h}
	}

	return add(base.array or _npcs,setmetatable(base,{
		-- sub-classing
		__index={
		-- coords in world units
		x=x,
		y=y,
		z=0,
		draw=function(self,x0,y0,z0,a0)
			local ca,sa=cos(a0),-sin(a0)
			local x1,y1=self.x-x0,self.y-y0
			local scale=(self.z+8)/(z0+8)
			-- position in screen space (map units)
			x1,y1=scale*(ca*x1+sa*y1)+8,scale*(-sa*x1+ca*y1)+14
			
			ca,sa=cos(angle-a0),-sin(angle-a0)
			local outcode=0xffff
			for _,g in pairs(quad) do
				-- rotate in local space
				local ix,iy,code=g.ix,g.iy,0
				-- translate to cam space
				ix,iy=scale*(ca*ix-sa*iy)+x1,scale*(sa*ix+ca*iy)+y1
				if ix>14 then code=2
				elseif ix<2 then code=1 end
				if iy>16 then code|=8
				elseif iy<0 then code|=4 end
				outcode&=code
				-- to screen space
				g.x=ix<<3
				g.y=iy<<3
			end
			-- visible?
			self.visible=false
			if outcode==0 then
				-- used to stop firing when not visible!
				self.visible=true
				if(hit_t>0) memset(0x5f01,0x7,15) palt(0,true)
				tquad(quad,self.uv)
				if(hit_t>0) pal()
				if self.state then
					print(self.state,8*x1+8,8*y1,2)
				end
			end
			--[[
			if self.input then
				local ca,sa=cos(angle),sin(angle)
				local x0,y0=8*self.x,8*self.y
				local x1,y1=8*self.input.x,8*self.input.y
				line(x0,y0,x1,y1,11)
				x1,y1=x0+16*ca,y0+16*sa
				line(x0,y0,x1,y1,8)
			end
			]]
		end,
		die=function(self)
			local x,y=self.x,self.y
			make_part(small_blast_cls,x,y)
			do_async(function()
				wait_async(3)
				make_crater(x,y,angle)
			end)
			self.dead=true
		end,
		hit=function(self,dmg)
			self.hp-=dmg
			hit_t=2
			if self.hp<=0 then
				-- 
				self:die()
				return
			end
		end,
		update=function(self)
			hit_t-=1

			local move,target_angle=self:control()
			if target_angle then
				angle=target_angle
			end
			if move then
				assert(self.w,"invalid w for:"..name)
				assert(self.h,"invalid w for:"..name)

				-- prev pos
				_npc_map[to_npc_map(self.x,self.y)]=nil

				acc+=self.acc
				local dx,dy=acc*cos(angle),-acc*sin(angle)

				-- update pos
				local xarea,yarea=get_area(self,dx,0),get_area(self,0,dy)
				local xother,yother=solid_npc(self,dx,0),solid_npc(self,0,dy)
	
				-- solid?
				if not xother and band(xarea,0x1)==0 then
					self.x+=dx
				else
					acc=0
				end
				if not yother and band(yarea,0x1)==0 then
					self.y+=dy
				else
					acc=0
				end
				_npc_map[to_npc_map(self.x,self.y)]=true

				-- friction
				acc*=self.friction
			end
		end,
		collide=function(self,p)
			-- to npc base
			local px,py=p.x-self.x,p.y-self.y
			-- rotate
			local ca,sa=cos(angle),-sin(angle)
			px,py=ca*px+sa*py,-sa*px+ca*py
			
			local col,a,b,x0,y0,y1=collide(
				base.sprite,
				{sx=8*px-4,sy=8*py-4,sw=8,sh=8,hitmask=p.hitmask})
			--[[
			rect(64-16,64-16,64+16,64+16,11)
			circfill(64+8*px,64-8*py,2,12)
			if col then
				local ax,ay=flr(a.sx),flr(a.sy)
				rect(64+ax+x0,64-(ay+y0),64+ax+a.sw-1,64-(ay+y1-1),8)
				print(px.."\n"..py,64+ax+a.sw+4,64-(ay+y0),11)
				flip()
			end
			]]
			if col and intersect_bitmasks(a,b,x0,y0,y1) then
				-- assert(false)
				--local ax,ay=flr(a.sx),flr(a.sy)
				--p.rect={ax+x0,ay+y0,ax+a.sw-1,ay+y1-1}
				return true
			end
		end
	}}))
end
-- create actors
local npc_id=0
local light_tank_sprite=with_hitmask(make_sprite(64,16,16))

function make_tank(x,y)
	local angle,reload_ttl,states,state=0,25
	
	local function fire_bullet(self)
		if reload_ttl<0 and self.visible then
			local ca,sa=cos(angle),-sin(angle)
			make_bullet(small_bullet_cls,self.x+ca,self.y+sa,0,ca/2,sa/2)
			reload_ttl=25
		end
	end

	states={
		-- rotate
		function(self)
			local ttl,target_angle=10,angle+0.25-rnd(0.5)
			local think="?"
			-- if close enough, lock on player
			local x,y,px,py=self.x,self.y,plyr.x,plyr.y
			if dist(x,y,px,py)<8 then
				target_angle=atan2(px-x,-py+y)
				-- shortest angle
				local dtheta=target_angle-angle
				if dtheta>0.5 then
					angle+=1
				elseif dtheta<-0.5 then
					angle-=1
				end				
				think="☉"
			end

			return function()
				ttl-=1
				if(ttl<0) state=rnd(states)(self)
				angle=lerp(angle,target_angle,0.1)
				--self.state=think.."\n"..angle.."\n"..target_angle
				return false,angle
			end
		end,
		-- move
		function(self)
			local ttl=10+rnd(10)
			return function()
				ttl-=1
				if(ttl<0) state=rnd(states)(self)
				return true,angle
			end
		end,
		-- wait
		function(self)
			local ttl=5+rnd(10)
			return function()
				ttl-=1
				if(ttl<0) state=rnd(states)(self)
				return false,angle
			end
		end,
		-- fire
		function(self)
			return function()
				local x,y,px,py=self.x,self.y,plyr.x,plyr.y
				if dist(x,y,px,py)<8 then
					local target_angle,angle=atan2(px-x,-py+y),angle
					-- shortest angle
					local dtheta=target_angle-angle
					if dtheta>0.5 then
						angle+=1
					elseif dtheta<-0.5 then
						angle-=1
					end	
					if abs(target_angle-angle)<0.1 then
						--self.state="fire: "..abs(target_angle-angle)
						fire_bullet(self)
					end
				end
				state=rnd(states)(self)
			end
		end
	}

	local tank={
		w=0.4,
		h=0.4,
		sprite=light_tank_sprite,
		uv={
			0,0,
			2,0,
			2,2,
			0,2},
		hp=1,
		acc=0.008,
		friction=0.9,
		side=1,
		score=10>>16,
		control=function(self)
			reload_ttl-=1
			-- idle?
			if not state and dist(self.x,self.y,plyr.x,plyr.y)<20 then
				state,reload_ttl=rnd(states)(self),30
			end
			if(state) return state(self)
		end
	}

	return make_npc(tank,x,y,0,"tank")
end

local heavy_tank_sprite=with_hitmask(make_sprite(182,32,16))
function make_heavy_tank(x,y)
	local angle,acc,move_t=0,0.1,0
	local dx,dy=0,0
	-- can npc move?
	local id=npc_id
	npc_id+=1

	local tank={
		w=0.8,
		h=0.8,
		sprite=heavy_tank_sprite,
		uv={
			0,3,
			4,3,
			4,5,
			0,5},
		hp=10,
		acc=0.002,
		side=1,
		score=50>>16,
		-- co-routine data
		seek_dly=60,
		path={},
		control=function(self)
			return false,0
		end
	}

	return make_npc(tank,x,y,0,"heavy tk")
end

local msl_tank_sprite=with_hitmask(make_sprite(178,32,16))
function make_msl_tank(x,y)
	local angle,acc,move_t,reload_ttl=0,0.1,0,0
	local dx,dy=0,0
	-- can npc move?
	local id=npc_id
	npc_id+=1

	local tank={
		w=0.8,
		h=0.8,
		sprite=msl_tank_sprite,
		uv={
			4,3,
			8,3,
			8,5,
			4,5},
		hp=8,
		side=1,
		score=50>>16,
		-- co-routine data
		seek_dly=60,
		path={},
		control=function(self)
			reload_ttl-=1
			if reload_ttl<0 then
				reload_ttl=30
				do_async(function()
					for i=1,4 do
						wait_async(5)
						-- make_homing_msl(self.x,self.y,0)
					end
				end)
			end
			return false,0
		end
	}

	return make_npc(tank,x,y,0,"msl tk")
end

local heavy_turret_sprite=with_hitmask(make_sprite(76,32,24))
function make_heavy_turret(x,y)
	local toward=make_lerp_angle(0,0.1)
	local reload_delay,reload_ttl=_turrets*40
	-- records total number of turrets
	-- used to terminate current level
	_turrets+=1
	local turret={
		uv={
			2,0,
			6,0,
			6,3,
			2,3},
		hp=20, --debug
		side=1,
		score=500>>16,
		sprite=heavy_turret_sprite,
		die=function(self)
			_turrets-=1
			self.dead=true
			local x,y=self.x,self.y
			make_part(large_blast_cls,x,y)
			do_async(function()
				wait_async(10)
				-- todo: optimize for tokens
				for i=-2,0,2 do
					for j=-2,0,2 do
						map_set(x+i,y+j,217)
						map_set(x+i+1,y+j,218)
						map_set(x+i,y+j+1,233)
						map_set(x+i+1,y+j+1,234)
					end
				end
			end)
		end,
		control=function(self)
			-- account for turret size
			local x,y=self.x,self.y
			local angle=toward(x,y,plyr.x,plyr.y)
			-- close enough?
			-- todo: move activate as a shared property
			if not reload_ttl and dist(x,y,plyr.x,plyr.y)<20 then
				reload_ttl=reload_delay
			end
			-- not active
			if(not reload_ttl) return false,angle

			reload_ttl-=1
			if reload_ttl<0 then
				reload_ttl=40
				local ca,sa=cos(angle),-sin(angle)
				-- center position
				local w,cx,cy=1.5/3,x+2*ca,y+2*sa
				for i=-1,1 do
					do_async(function()
						wait_async((i+1)*10)
						-- still valid?
						if(self.hp<=0) return 
						make_bullet(large_bullet_cls,cx-i*sa*w,cy+i*ca*w,0,0.12*ca,0.12*sa)				
					end)
				end
			end
			return false,angle
		end
	}
	return make_npc(turret,x,y,0,"heavy turret")
end

-- blast marker
function make_crater(x,y,angle)
	local crater={
		array=_blasts,
		sh=16,
		sw=16,
		uv={
			7,1,
			9,1,
			9,3,
			7,3},
		hp=1}
	return make_npc(crater,x,y,angle,"crater")
end

-- static 4 direction turret
function make_static_turret(x,y)
	local reload_ttl=0,0
	local turret={
		sh=16,
		sw=16,
		hp=1,
		side=1,
		score=10>>16,
		draw=function()
			-- built-in map
		end,
		die=function(self)
			self.dead=true
			local x,y=self.x,self.y
			make_part(small_blast_cls,x+1,y+1)
			-- todo: move to die override
			do_async(function()
				wait_async(4)
				map_set(x,y,74)
				map_set(x+1,y,75)
				map_set(x,y+1,90)
				map_set(x+1,y+1,91)
			end)
		end,
		collide=function(self,p)
			-- to npc base
			local x,y=self.x,self.y
			local dx,dy=p.x-x,p.y-y
			-- todo: account for bullet thickness
			return dx*dx+dy*dy<2
		end,
		control=function(self)
			reload_ttl-=1
			-- close enough?
			if reload_ttl<0 and dist(x,y,plyr.x,plyr.y)<13 then
				reload_ttl=30
				do_async(function()
					for i=1,4 do
						wait_async(3)
						-- still alive?
						if(self.hp<=0) return						
						sfx(5)
						for i=0,0.75,0.25 do
							local ca,sa=cos(i),-sin(i)
							make_bullet(small_bullet_cls,x+0.5*ca+1,y+0.5*sa+1,0,0.2*ca,0.2*sa)				
						end
					end
				end)
			end
		end
	}
	return make_npc(turret,x,y,0,"turret")
end

-- hidden missile silo
function make_msl_silo(x,y)
	local hidden=true
	local turret={
		sh=8,
		sw=8,
		hp=1,
		draw=function()
			-- built-in map
		end,
		-- cannot be hit
		hit=function()
		end,
		-- cannot collide with
		collide=function()
		end,
		control=function(self)
			-- close enough?
			if hidden and dist(x,y,plyr.x,plyr.y)<12 then
				hidden=nil
				do_async(function()
					-- display silo
					for i=0,2 do
						map_set(self.x,self.y,163+i)
						wait_async(5)
					end
					-- fire msl
					make_msl(self.x+0.5,self.y+0.5)
					-- debug
					-- hidden=true
				end)
			end
		end
	}
	return make_npc(turret,x,y,0,"silo")
end

-- missile
function make_msl(x,y)
	-- get direction
	local ttl,dz,angle=0,0.1,atan2(plyr.x-x+0.5,-plyr.y+y+0.5)

	local msl={
		sw=8,
		sh=8,
		w=0.4,
		h=0.4,
		acc=0.2,
		friction=1,
		collide=function() end,
		control=function(self) 
			ttl+=1
			local du=min(2,flr(ttl/15))
			-- todo: z/dz
			self.uv={
				6+du,0,
				7+du,0,
				7+du,1,
				6+du,1}
			self.z+=dz
			if(ttl<0 or self.z<0) self:die() return
			dz+=gravity
			return true,angle 
		end
	}
	return make_npc(msl,x,y,0,"msl")
end

function make_homing_msl(x,y,angle)
	-- get direction
	local ttl,angle_ttl,acc,toward=40,20,0.4,make_lerp_angle(angle,0.05)
	local msl={
		w=0.4,
		h=0.4,
		sw=8,
		sh=8,
		uv={
			1,2,
			2,2,
			2,3,
			1,3},
		acc=0.04,
		friction=0.87,
		collide=function() end,
		control=function(self) 
			ttl-=1
			if(ttl<0) self:die() return
			
			angle_ttl-=1
			-- homing mode?
			if angle_ttl<0 then 
				angle=toward(self.x+0.5,self.y+0.5,plyr.x,plyr.y)
			end
			return true,angle 
		end
	}
	return make_npc(msl,x,y,0,"homing msl")
end

-->8
-- map & draw helpers
#include includes/tquad.lua

-- check for the given tile flag
function fmget(x,y)
	return fget(_map[(x\1)|(y\1)<<7])
end
-- return true if solid tile for a bullet
-- 0x1 + 0x2
function bullet_solid(x,y)
	return band(fget(_map[(x\1)|(y\1)<<7]),3)==3
end

function get_area(a,dx,dy)
	local x,y,w,h=a.x+dx,a.y+dy,a.w,a.h
	return 
		fmget(x-w,y-h)|
		fmget(x+w,y-h)|
		fmget(x-w,y+h)|
		fmget(x+w,y+h)
end

function map_set(i,j,s)
	-- no need to track 'static' tiles
	if(fget(s)!=0) _map[i|j<<7]=s
	-- cell coord (128x128)->(4x4)
	local ck=i\32+(j\32<<2)
	local cell_map=_cells_map[ck]
	-- cell is 4*dword with a stride of 128/4 = 32
	-- cell entry is packed as a dword
	local k=(band(i,31)\4<<2)+(band(j,31)<<7)
	-- shift 
	local shift=(i%4)<<3
	cell_map[k]=(cell_map[k] or 0)&rotl(0xffff.ff00,shift) | (0x0.0001<<shift)*s

	-- invalidate cache
	for _,entry in pairs(_map_lru) do
		if(entry.k==ck) entry.k=-1 entry.t=-1
	end
end

function draw_map(x,y,z,a)
	local ca,sa=cos(a),-sin(a)
	local scale=8/(z+8)
	-- project all potential tiles
	for i,g in pairs(_grid) do
		-- to cam space
		local ix,iy,outcode=((i%5)<<5)-x,(i\5<<5)-y,0
		ix,iy=scale*(ca*ix+sa*iy)+8,scale*(-sa*ix+ca*iy)+14
		if ix>14 then outcode=2
		elseif ix<2 then outcode=1 end
		if iy>16 then outcode|=8
		elseif iy<0 then outcode|=4 end
		-- to screen space
		g.x=ix<<3
		g.y=iy<<3
		g.outcode=outcode
	end
	
	-- collect visible cells
	local viz={}
	for k,cell in pairs(_cells) do
		-- visible or partially visible?
		if(cell[1].outcode&
			 cell[2].outcode&
			 cell[3].outcode&
			 cell[4].outcode)==0 then
			viz[k]=cell
		end
	end
	
	-- draw existing cache entries
	for i,entry in pairs(_map_lru) do
		local cell=viz[entry.k]
		if cell then
			local offset=i<<5
			tquad(cell,{
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
		local mem=0x2000+(mini<<5)
		for base,v in pairs(_cells_map[k]) do
			poke4(mem+base,v)
		end
		-- draw with fresh cache entry		
		local offset=mini<<5
		tquad(cell,{
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

	-- craters
	palt(0,false)
	palt(3,true)
	for _,crater in ipairs(_blasts) do
		crater:draw(x,y,z,a)
	end	
	palt()
end

-->8
-- init/update/draw/states
local _state

function draw_hud(score,lives,ttl)
	printb("SCORE",18,0)

	local s=score_tostr(score)
	printb(s,18,6)
	
	printb("HI",100,0)

	printb("99999",82,6)

	printb("TIME",54,0)

	-- frames to seconds
	local t=tostr(ttl\30)
	printb(t,60,6)

	for i=0,lives-1 do
		spr(166,18+i*6,120)
	end	
end

function play_state()
	-- restore map
	reload()
	-- todo: time to complete from level (+ difficulty)
	local score,lives,ttl=0,3,90*30
	-- reset
	_npcs,_parts,_bullets,_blasts,_turrets={},{},{},{},0
	-- exit path
	local exit_path={}

	-- init maps
	-- collision map
	_map,_cells,_cells_map,_grid,_map_lru,_npc_map={},{},{},{},{},{}
	-- 
	local grid_w=4
	-- grid_w intervals = grid_w+1^2 points
	for i=0,(grid_w+1)*(grid_w+1)-1 do
		_grid[i]={x=0,y=0,outcode=0}
	end
	-- direct link to grid vertices?
	for i=0,grid_w*grid_w-1 do
		-- cell coords in grid space 
		-- +1: account for 'additional' ^2+1 point
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
		if fget(s,2) then
			jumppads[i|j<<7]=3
		end
		-- turrets: encoded in the last 4 bits
		local f=band(fget(s),0xf0)
		if f==0xc0 then
			make_static_turret(i,j)
		end

		if(s==0 and rnd()>0.90) s=flr(24+rnd(4))
		-- update map
		map_set(i,j,s)
	end)
	-- decompress actors sprite "map"
	-- avoid overwriting ram while reading..
	local tmp={}
	mem=decompress(mem,function(s,i,j)
		if(s!=0) add(tmp,function() mset(i,j,s) end)
	end)
	for _,t in pairs(tmp) do
		t()
	end

	local actor_factory={
		[1]=make_tank,
		[2]=make_tank,
		[3]=make_tank,
		[4]=make_tank,
		[5]=make_heavy_turret,
		[17]=make_msl_tank,
		[18]=make_msl_tank,
		[19]=make_msl_tank,
		[20]=make_msl_tank,
		[48]=function(x,y,a) plyr=make_plyr(x,y,64,a) end,
		[49]=make_msl_silo,
		[50]=function(x,y) exit_path[1]={x=x,y=y} end,
		[51]=function(x,y) exit_path[2]={x=x,y=y} end
	}

	local n=peek2(mem)
	mem+=2
	for i=1,n do
		local id,x,y,a=peek(mem),peek(mem+1),peek(mem+2),peek(mem+3)
		local fn=actor_factory[id]
		assert(fn,"unknown actor id:"..id.."@"..x.."/"..y)
		fn(x,y,a)
		mem+=4
	end
	assert(plyr,"missing player start pos")

	return {
		draw=function()
			local px,py,pz,pangle=plyr:get_pos()

			-- blinking lights
			pal(8,picknext(red_blink,0.5)) 
			draw_map(px,py,pz,pangle)
			pal()

			-- draw
			for _,npc in ipairs(_npcs) do
				npc:draw(px,py,pz,pangle)
			end

			plyr:draw()
		
			draw_parts(_bullets,px,py,pz,pangle)
			draw_parts(_parts,px,py,pz,pangle)
			
			--line(64,64,64+16*ca,64+16*sa,11)
			--line(64,64,64-16*sa,64+16*ca,8)
	
			draw_hud(score,lives,ttl)
		end,
		update=function()
			-- todo: loose live if timeout
			ttl-=1

			cam_update()

			if _turrets==0 then
				_state=exit_level_state(score,lives,ttl,exit_path)
			end

			plyr:update()

			-- update actors
			for _,npc in ipairs(_npcs) do
				npc:update()
				if npc.dead then
					-- any points?
					if(npc.score) score+=npc.score
					del(_npcs,npc)
				end
			end
		
			update_parts(_bullets)
			update_parts(_parts)
		end
	}
end

function exit_level_state(score,lives,ttl,path)
	local sprite,fly_mode=96
	-- clear up baddies and bullets
	_npcs,_bullets={},{}
	
	local x,y,z,angle=plyr:get_pos()

	-- initial target point
	local n=make_v(path[1],path[2])
	local d=sqrt(n.x*n.x+n.y*n.y)
	n={x=n.x/d,y=n.y/d}
	-- project position into line
	local p=make_v(path[1],plyr)
	d=p.x*n.x+p.y*n.y

	local wp={x=path[1].x+d*n.x,y=path[1].y+d*n.y,next=path[2]}
	-- exit level animation
	do_async(function()
		-- go to waypoint	
		local dx,dy
		while wp do
			local target_angle=atan2(wp.x-x,-wp.y+y)+0.25
			-- shortest angle
			local dtheta=target_angle-angle
			if dtheta>0.5 then
				angle+=1
			elseif dtheta<-0.5 then
				angle-=1
			end	

			-- rotate toward
			while(abs(angle-target_angle)>0.01) do
				angle=lerp(angle,target_angle,0.1)
				yield()
			end
			angle=target_angle
			-- move toward
			dx,dy=-sin(angle)*0.1,-cos(angle)*0.1
			while(dist(x,y,wp.x,wp.y)>0.1) do
				x+=dx
				y+=dy
				yield()
			end

			-- next way point
			wp=wp.next
		end

		-- accelerate
		wait_async(24,function()
			x+=dx
			y+=dy
			dx*=1.08
			dy*=1.08
		end)
		-- dive
		sprite=215
		wait_async(10)
		sprite,fly_mode=247,true
		local dz=-0.1
		wait_async(45,function()
			z+=dz
			dz-=0.01
		end)		 
		-- back to normal
		sprite,fly_mode=217
		wait_async(10)

		-- todo: load next cart
		_init()		 
	end)

	return {
		draw=function()
			if z>-8 then
				pal(8,picknext(red_blink,5.3)) 
				draw_map(x,y,z,angle)
				pal()
			end

			spr(sprite,56,fly_mode and 108 or 106,2,fly_mode and 1 or 2)
			if fly_mode and flr(time()*16)%2==0 then
				local r=3+rnd()
				circfill(57,110,r,10)
				circfill(70,110,r,10)
			end

			--[[
			if ttl<30 then
				printb("stage 01 clear",20,63)
			elseif ttl<60 then
				printb("time bous",32,63)

				printb("80*30 points",32,73)
			end
			]]
		end,
		update=function()
		end
	}
end

function _init()
	-- extend tline limits
	poke(0x5f38,128)
	poke(0x5f39,128)
	_state=play_state()
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

	_state:update()

	time_t+=1
end

function _draw()
	cls()
	-- vertical display
	clip(16,0,128-32,128)

	_state:draw()

	-- swamp green
	pal(11,138,1)
end

__gfx__
0000000033333333000000055ddddd5511000000dddddddddddddddddddddddd000000000000000000000000000000001111499a4112442442442114a9941111
0000000035333333000001ddd66666dd55115100dd77d77d77d77d77d77d77dd000000000000000000000000000000001777499a4771221221221774a9947771
00000000333333330000155566dd6dd6d567d510d7dddddddddddddddddddd7d0000000000000000000000000000000015665577511244244244211577556671
0000000033333333000151567775d67d56667d51d7dddddddddddddddddddd7d000000000000000000000000000000001566567d5661241441421665d7656671
000000003333353300155567dddd5d15d5d66751dddddddddddddddddddddddd0000000000000000000000000000000044556555666612244221666655565544
000000003353333300155dd65d1155155555dd10d7dddddddddddddddddddd7d00000000000000000000000000000000aa7d5dddddddd111111dddddddd5d7aa
0000000033333333015555615115555111556751d7dddddddddddddddddddd7d0000000000000000000000000000000099d75d11111111111111111111d57d99
0000000033333333105156111551111111155651dddddddddddddddddddddddd0000000000000000000000000000000099dd5d1dd51111111111115dd1d5dd99
33333333333333331551d6d5333333335551d655d7dddddddddddddddddddd7d0000000000000000000000000060000044556d1d5111111111111115d1d65544
33333333333333331115d66d333333335115d6d5d7dddddddddddddddddddd7d0070000000000000000000000676000015666d15111115555551111151d66651
33331652163333331115d76d333333331115d765dddddddddddddddddddddddd0000000000000000000000000060000015666d11111155555555111111d66651
333156d817633333151d766d33333333151d7665d7dddddddddddddddddddd7d0000000000000000000000000000000021216d11111555555555511111d61212
3315d75257663333155d666d33333333155d66d5d7dddddddddddddddddddd7d0000000010000000000000000000000042421d11115555555555551111d12424
3155d77577776333155d666533333333155d6dd5dddddddddddddddddddddddd0000000000000000000000000000000042442111155555555555555111124424
3111166667511333151176d53333333315117d55d7dddddddddddddddddddd7d0000000000000070000000100000000021212111155555555555555111121212
11288d66658823331555d765333333335555d755d7dddddddddddddddddddd7d0000000000000000000000000000000042444111155555555555555111144424
11111666675d53330151d6d33dddd53311d666d1dddddddddddddddddddddddd3333333333333333333553330000000042444111155555555555555111144424
115dd56d6777633301556dddd66666dddd6667d1d7dddddddddddddddddddd7d3156d633333333333353b5330000000021212111155555555555555111121212
1115dd58576613330151d76d6676666666667d51d7dddddddddddddddddddd7d15dd7d633333333335db3bd30000000042442111155555555555555111124424
311155d816d133330015556dddd7666666dd5510dddddddddddddddddddddddd1dd66763333333315dd3b7bd0000000042421d11115555555555551111d12424
331115d21d13333300155155155ddd151555d100d7dddddddddddddddddddd7d15dd66d33333333155d73b350000000021216d11111555555555511111d61212
3331111111333333000115155111551551155100d7dddddddddddddddddddd7d155dd5d3333333115d537dbd0000000015666d11111155555555111111d66651
3333333333333333000001115115555515551000dd77d77d77d77d77d77d77dd11555d533333331115d5d5d30000000015666d15111115555551111151d66671
3333333333333333000000005511111101110000dddddddddddddddddddddddd3111153333333111115d5d330000000044556d1d5111111111111115d1d65544
dddddddd3333333335333553dddddddd1111111111111111ddddddd1d0addddddddddddd33333111111113330000000099dd5d1dd51111111111115dd1d5dd99
dddddd7d3333333333335675ddd77ddd111111111111111ddddddd11d00ddddddd282ddd33331111111333330000000099d75d11111111111111111111d57d99
ddddd77d333533333333d565ddd77ddd11111111111111ddddddd111da0dddddddd282dd333311111333333300000000aa7d5dddddddd111111dddddddd5d7aa
dddd7d7d33d7633336333d5dddd77ddd1111111111111ddddddd1111daaddddddddd282d33333113333333330000000044556555666612244221666655565544
dddd7d7d333d3333576333d3ddd77ddd111111111111ddddddd11111d0addddddddd282d3333333333333333000000001566567d5661241441421665d7656651
ddddd77d33333333d5333333ddd77ddd11111111111ddddddd111111d00dddddddd282dd33333333333333330000000015665577511244244244211577556671
dddddd7d5633335333336733ddd77ddd1111111111ddddddd1111111da0ddddddd282ddd3333333333333333000000001555499a4551221221221554a9945571
dddddddd3533333333535d33dddddddd111111111ddddddd11111111daaddddddddddddd3333333333333333000000001111499a4112442442442114a9941111
0000000000000000000000000066000000000000d555d50000007777700000000888880000000000333333333333333300000000000000000000000000000000
0000000000000000002222000d55d00000000000d56d6d0000007000700000000088800000000000333033003030333300000000000000000000000000000000
0000000000000000022ee220d511ddddddd000006d6dd50000000000000000000008000000000000333330850033333300000000000000076d50000000000000
000245d5d5d9000002e77e2065dd6666666d00006dd56d00000000000000000000000000000000003303305280033033000000000000067766d5500000000000
00184555554da00002e77e207d77f777777f0000d56d6d00770000000007780000080000080000003333042014000333000000000000677766dd550000000000
0012244996549000022ee220fffffffffff000006d6dd5007000000000007880008080008800000033304d600100403300000000007777766666666d00000000
0012499ffff200000022220000660000000000006dd56d0070000070000078880800080888000000303085d8024da0330000000007666667777666d500000000
001292d9af616d000000000000660000000000000000000070000000000078800080800088000000330880820285d003000000006766666666677d57799a7700
001292d9af75770000000000006600000000000000000000770000000007780000080000080000003304d0000028003300000006776666666666656664446600
0012499ffff2000000000000005500000000000000000000000000000000000000000000000000003005a808d002821300000007776666666666655111111100
001224499654900000099000d511ddddddd000000000000000000000000000000008000000000000330140059400013300000067776666666666655500000000
00184555554da000009a790065dd6666666d0000000000000000700070000000008880000000000033331002502803330000007667666666666665d7799a7700
000245d5d5d90000019aa9007d77f777777f000000000000000077777000000008888800000000003333300800823333000000d6676666666666656664446600
000000000000000001199000fffffffffff00000000000000000000000000000000000000000000033033100000303330000005d676666666666655111111100
00000000000000000011000006666000000000000000000000000000000000000000000000000000333333013033333300000005d76666666666655500000000
0000000000000000000000000066000000000000000000000000008888800000000000000000000033333333333333330000000557666666666665d7799a7700
0000000000000000000000000000000000000000000000000000000888000000000000003366677667766776677666330000000057666666666dd56664446600
0000000000000000000000000000000000000000000000000000000080000000000000003676d776d776d776d7766763000000000766dddddddddd5111111100
0000000d6000000000056000000000000000000000000000000000000000000000000000d771111111111111111117760000000000ddddddddddddd500000000
0000090d709000000056760000000000000000000000000000000000000000000000000016d11dd11dd11dd11ddd11d60000000000005dd55551110000000000
00009a9d69a900000056760000000000000000000000000000000000000000000000000066ddddddddddd4adddddd16600000000000005dd5511100000000000
0000d5f5df5d00000015650000000000000000000000000080000088888000008000000077dddddddddd149dddddd177000000000000000d5110000000000000
0000d596795d00000056760000000000000000000000000088000080008000088000000077dddddddddd11dddddd117700000000000000000000000000000000
00006df88fd6000000000000000000000000000000000000888000800080008880000000d6dddddddddddddddddd11d600000000000000000000000000000000
00056df5dfd65000000cc000dddddddddddddddddddddddd88000080008000088000000066ddddddddddddddddddd16600000000000000000000000000000000
000fd596795df00000c7cc00d777777dddddddddd7dddddd80000088888000008000000077ddddddddddddddddddd17700000000000000000000000000000000
000f6d4664d6f00001cc7c00dd7dd7ddddddddddd77ddddd00000000000000000000000077dddddddddddddddddd117700000000000000000000000000000000
0009d549f95d9000011cc000ddd77dddddddddddd7d7dddd000000000000000000000000d6dddddddddddddddddd11d600000000000000000000000000000000
00049f1491f9400000110000ddddddddddd77dddd7d7dddd00000000000000000000000066ddddddddddddddddddd16600000000000000000000000000000000
000148111184100000000000dddddddddd7dd7ddd77ddddd000000008000000000000000677dddddddddddddddddd77600000000000000000000000000000000
000011000011000000000000ddddddddd777777dd7dddddd0000000888000000000000001d76677667766776677667d100000000000000000000000000000000
000000000000000000000000dddddddddddddddddddddddd0000008888800000000000001156d776d776d776d776d51100000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000d666d66dd66d666732233223233322333443344343334433
000000000000000000000000000000000000000000000000000000000000000000000000000000005d666660066666762bb22222222222224334444444444443
0000000d600000000000000d600000000000000d600000000000000000000000000000000000000055d666666666676613b22bb2232222223334433443444444
0000009d7900000000000004900000000000009d7900000000000000000000000000000000000000555dddddddddd666112213b2332bb2233333433433433443
000009a96a9000000000004994000000000009a69a90000000000000000000000000000000000000555766500766d666322211222213b2233444444444333443
000006dfdd6000000000005d75000000000006ddfd60000000000101101000000000000000000000555765500776d666322bb222221122233443344444334443
00000d5975d00000000000d56d00000000000d5795d0000000001915d19100000000000110000000555155d66a77d6663213b2bb222233223433343344443344
000006df8d6000000000005675000000000006d8fd60000000009a9d69a900000000010110100000d05100600600d60d2211213b222323224433433344434344
000056dfdd650000000000d565000000000056ddfd65000000006df5dfd600000000111111110000d05100600600d60d2bb22112222222224334433444444444
0000fd5975df000000000059f90000000000fd57955f00000000d596795d00000000141661410000555155d66a77d66613b22222222332223334444444433444
0000f6d46d6f0000000000dfff0000000000f6d64d6f0000000d6df5dfd6d00000014a4d64a410005551155005d6d66d11223332222333233344333444433343
000096d4fd69000000000059f9000000000096df4d6900000006d5f28f5df000000d5ff5dff5d000555711500566d666222223322bb223224444433443344344
000049f49f9400000000002494000000000049f94f94000000096d4674d6900000096d45d9d69000555771111177d66d322bb22213b222223443344433344444
00000481184100000000001241000000000014810840000000019f1491f9100000019f1281f910005555555555555d663213b222112222233433344433444443
0000001011100000000000011000000000000110000000000000110110110000000011011011000055555550055555d622112322222222224433334444444444
000000000000000000000000000000000000000000000000000000000000000000000000000000005525255dd555555d32333322233332333433334443333433
0000000000000000000000003333333333333333335bbb330000000000000000113333333333333133333311333333111111111111111111dd566666677776dd
0070000000d70000007000003333333333b5bb3335b5bbb30060000000000000113333333333331133333311333333111111111111111113d56ddd5d5555576d
0d600000006600000d600000333bb33335577bb351577bbb0d9d00000000000033333333333333113333531133333333333333333333333316d0000000000576
077760000977777009777d0033566b3335d667b315d887bb9d2d9000000000003333333333333311333333113333333353333333533335331600000000000057
9fff7f009afffff69a9ff600335d6b3335d667b311d887bb9d4d9000000000003333333333333311333333113333333333533333333333331600000000000057
066660000966666009666d0033155333315ddbb3151ddbbb19191000000000003333333333333311333533113333333333333333333333331600000000000057
0d600000006600000d600000333333333315b5331151b5b301010000000000003333333333333311333333113333333333333533533333331600000000000057
0070000000d7000000700000333333333333333311151b3300000000000000003333333333333311333333113333333333333333333333331600000000000056
33333333333333330000111010111110011110111ddd1000112222224420022211224100000000003333333333333333000000000000000016000000000000d6
3333315555dd533300002865656d661005667d6667692000128999997a951999a499a91000000000333333333333333300000000000000001600000000000056
33331555d567d533000028666dd5d61005667d66676920001289a9a97a944999a499a991000000003333333335b63333000000000000000016000000000000d6
3331515656667d5300005d66dd55550001667d6677fd1000154a7a7777999999a499a9920000000033333d6331d63333000000000000000016000000000000d6
33155567d5d667d3000015d655111111551d65df6d1000000151d5d5d499911151559441000000003333db7633d3333300000000000000001700000000000056
33155dd65555ddd300000566dd15555706567d67661000000151d5d5d49912444442411000000000333db3b63423333300000000000000001570000000000561
315555611155d75300000566dd12555760567d67661000000151d5d5d4912499999415d1000000003331db32343333330000000000000000d15766666666661d
155156111115565300000566dd15555706567d67661000000155544999924999999ad6660000000033331244333333330000000000000000dd111111111111dd
1151d6d011d6665300000566dd15555760567d6766100000015554499994a999999a677700000000333333333433333333333333333333566633444333333333
11556ddddd66675300000566dd12555706567d67661000000151d5d5d49aa7999997566d00000000333335643425633333333333333335dddd63344333433333
1151d76d66667653000005666d15555760567d676d1000000151d5d5d499aa77777745100000000033335db63353b6333333333333435dd67dd6333344443333
1115556d666d6653000015d6ddddddd1551d65dfd510000001515151529999aaaaa99d4100000000333313d633155333333443344435dd5d6ddd633344433333
311551551666d53300005d666667661005667d66676d100015442424aa999999a499a99200000000333331133333333333344333445ddd55ddddd63333333333
3111151557d66533000028666667661005667d6667692000128949497a9aa999a499a9910000000033333333333333333334344335dddddddddddd6333433333
331111111dd65333000028656567661005667d6667692000128999997a945999a499a910000000003333333333333333344433335dddddddddddddd633444433
333111110d6533330000116161666d100d66d16666dd10001124446666410444d144410000000000333333333333333334433335dddddddddddddddd63344433
0000000033333dddddddddddddd3333300000000000000000000000000000000000000000005ddd100088d82000000003333435ddd8dddddddddd8ddd6334443
000000003333d55555555555555d333300000000000000000005500000000000000000000028dddd5004dddd00000000344335ddd88dddddddddd88ddd633443
00110000333d566b66666666b665d33300000000000000000000500000000000000000000dd2821d2005a8d80000000034435ddd888dddddddddd888ddd63333
0011000033d566bb6b66b6b6bb665d33000000000055000000000050000000000000000094ddd1ddd5004dd5000000003335dddddddddd8dd8dddddddddd6333
000001003d566bbbbbb6bb66bbb665d305500000055550000055005000000000000000005d28dd42dd5d1dd200000000335dddddddddd88d688dddddddddd633
00001100d56666bbbbbbbbbbbb66665d5555000000055000005500000000090d60900000dd82d4d65d5dddd80000000035dddddddddd888d6888dddddddddd63
01000000d56b66bbbbbbbbbbbb66b65d55550000000500000000000000009692896900005ddd085dddd5d1d0000000005dddddddddd8888678888dddddddddd6
00000000d56bbbbbbbbbbbbbbbbbb65d0550000000000000000000000000d595d95d00008ddd01885d526600000000001dd67ddddddddd688766dddddddd67d6
bbbbbbbbd566bbbbbbbbbbbbbbbbb65ddddddddd79977997dddddddd000fd546645df00025ddd1282d051560000000001d5d6ddddddd55d886ddddddddd5d6d6
b66bbbbbd56bbbbbbbbbbbbbbbb6665ddddddddd77997799dddddddd00096d49f9d690005ddddd11ddd0011d000000001d55ddddddd8888d68888dddddd55dd5
b66bb6bbd56bbbbbbbbbbbbbbbbb665ddddddddddddddddddddddddd00049f0490f94000ddd56ddd126dd00d0000000031dddddddddd8885d888dddddddddd53
bbbbb66bd56bbbbbbbbbbbbbbbbbb65dddddddddddddddddd777777d0005480000845000ddd1d66d28d5dd8500000000331dddddddddd885d88dddddddddd533
bbbbbbbbd566bbbbbbbbbbbbbbbb665dddddddddddddddddd777777d00005500005500008ddd1dd66881dd52000000003331dddddddddd8dd8dddddddddd5333
bb66bbbbd566bbbbbbbbbbbbbbb6b65ddddddddddddddddddddddddd000000000000000004dd011dd18dd4200000000033331ddd888dddddddddd888ddd53333
bb66bbbbd56bbbbbbbbbbbbbbbb6665d99779977dddddddddddddddd000000000000000000dd4000182d4d0000000000334331ddd88dddddddddd88ddd533443
bbbbbbbbd56bbbbbbbbbbbbbbbbbb65d79977997dddddddddddddddd0000000000000000000da000221d8000000000003443331ddd8dddddddddd8ddd5334344
00000000d56bbbbbbbbbbbbbbbbbb65ddddddd9777dddddd00000000000000d88d00000000000000000000000000000034443331dddddddddddddddd53444444
00000000d56b66bbbbbbbbbbbb66b65ddddddd9997dddddd0000000000006d95d9d60000000000000000000000000000334333331dddddddddddddd533443433
04044400d56666bbbbbbbbbbbb66665ddddddd7999dddddd0000000000049896798990000000000000000000000000003333333331dddddddddddd5334433333
0f4fff403d566bbbbbbbbbbbbbb665d3dddddd7779dddddd00000000000542249224d00000000000000000000000000033334433331dddddddddd53334433333
072777f033d566bbbb66bbb6bb665d33dddddd9777dddddd000000000000550220550000000000000000000000000000333344333331ddd67ddd533344433333
0f0fff00333d566b66666666b665d333dddddd9997dddddd0000000000000000000000000000000000000000000000003333333333331d5d6dd5344333333333
000000003333d55555555555555d3333dddddd7999dddddd0000000000000000000000000000000000000000000000003333333333333155dd53444333333333
0000000033333dddddddddddddd33333dddddd7779dddddd00000000000000000000000000000000000000000000000033333333333333111533333333333333
__label__
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000002111211121112111211121112111211121112111211121112111211121112111211121112111211121112111211121110000000000000000
00000000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000000
00000000000000001121112111211121112111211121112111211121112111211121112111211121112111211121112111211121112111210000000000000000
00000000000000001111111111111111111111111111111111111111116666666666111111111111111111111111111111111111111111110000000000000000
0000000000000000212121212121212121212121212126666d212121216ffffffff6212121666621212121212121212121212121212121210000000000000000
000000000000000012111211121112111211121112666ffff6d512111269f9f9f9f612111d6fff66121112111211121112111211121112110000000000000000
000000000000000021212121212121212121212166fff949496d522121649494949621215d6499ff612121212121212121212121212121210000000000000000
0000000000000000111211121112111211121116ff9994944446d52211644444444611125d69494961d666121112111211121112111211120000000000000000
000000000000000021212121212121212121216f4949444424246d5226424242424261225d64949465d69f662121212121212121212121210000000000000000
00000000000000001212121212121212121212649444424242226d5216222222222262125d64444965d6949f6d66121212121212121212120000000000000000
00000000000000002121212121212121666d56444424242422226d5226212121212161225d62424465d649496d6f612121212121212121210000000000000000
00000000000000001212121212121666f996d6424242222221216d5216111111111162125d64242465d644446d69f66212121212121212120000000000000000
00000000000000002222222222266f99494964242222221211116d5226101010101062225d62224265d642446d649ff622222222222222220000000000000000
000000000000000012121212126f9994944462422221211111116d5216010101010162125d62222265d624426d624946d6661212121212120000000000000000
00000000000000002222222226994944444462222212111116666d5226000000000062225d61212165d622246d642496d6ff6222222222220000000000000000
0000000000000000121212121694944242426212111116666ddddd5216100100100162125d61111165d622226d624246d699f662121212120000000000000000
000000000000000022222222264444242222611110106dddd555552260000000000006225d61010165d621216d642426d6949ff6622222220000000000000000
00000000000000002212666d564242422266610101006d522212221260000000000006125d60101065d611126d622246d649494ff61222120000000000000000
000000000000000022669f6d5624222666d5601000006d666d52222260000000000006225d60000065d601016d622426d62444949f6622220000000000000000
000000000000000026f9496d5622226d522260000000660006d5222260000000000006225d60000065d610106d612226d642424449ff62220000000000000000
0000000000000000d694946d5621216d5222600000000000006d522260000000000006225d60000065d600006d601126d62424244499f6620000000000000000
00000000000000002644446d5611106d666d6000000000000006d52600000066000000625d60000065d600006d6101165d66224242449ff60000000000000000
00000000000000002624226d56100006000660000000000000006d56000006dd600000625d60000065d600006d60101625dd6624242449960000000000000000
00000000000000002622216d56000000000060000000000000006d56000006dd600000625d60000065d600006d6000062225dd66224242460000000000000000
0000000000000000d6221116d6000000000066000000000000006d56000006dd600000625d60000065d600006d600006d2d255d6212124260000000000000000
000000000000000026110106d600000000006d600666000000006d5600000655600000625d60000065d600006d600006222225d6121266260000000000000000
000000000000000061000006d600000000006d5662d6000000006d6000000655600000065d60000065d600006d600006d2d225d611116d660000000000000000
000000000000000060000006d600000000006d522226000000006d6000000000000000065d60000065d600006d600006222d25d60101622d0000000000000000
000000000000000060000006d560066000006d52d2d6000000006d6000000000000000065d60000065d600006d600006d2d225d6000062d20000000000000000
000000000000000060060006d5266d6000006d522d26000000006d6000000000000000065d60000065d600006d6000062d2d25d600006d2d0000000000000000
000000000000000060060006d522d26000006d566666000000006d6000000000000000065d600000666600006d600006d2d225d6000062d20000000000000000
000000000000000060060006d52d2d60000066600000000000006d6000000000000000065d600000000000006d600006666d25d600006d2d0000000000000000
000000000000000060000006d5666660000060000000000000006d6000000000000000065d600000000000006d6000000006666600006dd20000000000000000
00000000000000006000000066000000000060000000000000006d6000000000000000065d600000000000006d6000000000000600006d2d0000000000000000
0000000000000000600000006000000000006000000000000000660000000666600000006d600000000000006d60000000000006000062dd0000000000000000
0000000000000000600000006000000000066000000000000006d60000006dddd600000065d60000000000065d6000000000000600006d2d0000000000000000
000000000000000060000000600000000006d60000000000006d560000006dddd6000000625d6000000000065d6000000000000600006ddd0000000000000000
00000000000000006006600060000000006d526000000000006d560000006d55d60000006d256000000000065d6000000000000600006d2d0000000000000000
00000000000000006006600060000000006d52600000000006d5260000006d55d60000006d25d600000000625d6000000000000600006ddd0000000000000000
0000000000000000666666666666666666d52dd6666666666d52d66666666d55d66666666dd25d66666666d25d6666666666666666666ddd0000000000000000
00000000000000006666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666660000000000000000
00000000000000006666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666660000000000000000
0000000000000000d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d60000000000000000
00000000000000006d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d6d0000000000000000
00000000000000005ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd0000000000000000
0000000000000000d5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5ddd5dd0000000000000000
00000000000000005d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d550000000000000000
0000000000000000d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d555d5550000000000000000
00000000000000000555055505550555055505550555055505550555055505550555055505550555055505550555055505550555055505550000000000000000
00000000000000005050505050505050505050505050505050505050505050505050505050505050505050505050505050505050505050500000000000000000
00000000000000000500050005000500050005000500050005000500050005000500050005000500050005000500050005000500050005000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000008888808888800000000000000000000000000000000000000000000
00000000000000000000000000000000000000000008888808888808888808888000888800088800088800000000000000000000000000000000000000000000
00000000000000000000000000000000000000000008888008808808888008808808880000088808888800000000000000000000000000000000000000000000
00000000000000000000000000000000000000000008880008888008880008808800088800088808880000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000008880008808808888808888008888000088808888800000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__gff__
00000303030000000000000001010101c10103000300000000000000010101010101030303000000030001000101a1010002020000000000000000000101010100000000000000000000020200000000000000000000000000000202000000000000000000000000000303030000000000000000000000000003030300000000
0000000000000000000003030202000000000000000000000000030302020000000000000002000000000000000001010303000000000000000000000000010103030000000000000000000000000000000000000000000000020200000000000000080000000000000202000000040000000000000000000000000000000000
__map__
ed0800804060503824160d0784426150b864361d0f8844625138a4562d178c46622d08e476350d8ec86452390c7e32d00100e5403024724d08944ae65339a4aa5b2f8aca182ac9e2b1812c684e2093a9ed168d479e3026f42884e8274fa82a82941a64029d50ac566b550a5552ab308e809835b09d763d0ab3c264362b25b6b5
52af57e0b319558edb3f95d2ee7299b5c60774badb86232188c40350c1e129f70b941a88ac56dba9f45aedee7749bd40b1f90c96270a0100e246432c5d4f1b43b664a9ed169349a75ccc802afb0bf6ceb3831884f0fbad1ef77dbdc3e334f9a09567775a68b81c4e2706bc2770915b34158b3586ed5ad16e775a2dfefb0dc2e1
d5b8b4fe9d6758d2e572f99ad69de265d74723f8feec0e23bbf7fc6fc63e0f0b43c7ba71dab7a5ea811cc34d46645e42788e7941351609791f9849f97f1c37f95071de772a03815cb381ad7b2186809e27e0d590016f9878a1fa779dc6f9656c58d340222c5e46ece1876058720438e186ea3f6824156ddc861fa679bd615bf6
1dd54895f8ce35562388e6538f23e9060190d846e1c7776477e5bb7d1795f92f93d583c63b952058f5e404cb008e6f9c271940139698a562137e1da64d47655559955099e049a21d380f3560b0091244763453e2f56e2b975a39267664a7d5321794669a12865428898d9aa319e89e908b6930ca5859215991ff53e52a6a1ea7
1a5a7d7fab1aa681d9776225b6aa49a98542aea1282ac6b25a2b56a9bc9e55895e58af11fafaadabdccb0ec45aac67da925665d64a26b351ab3c13a069a9ad59b7501b7d587727a8baa871a00545a6a5da999a83955ab7b2e578aba5421367eb66ee01a5abdbc959b029bafdcb38ef86c9ff89ae98b5879ba719be7371e58c05
27c0e01c16c2a64e2c26f04c1c595d5a7dd9f0069e48e8c9b164c5d166d95a3c4f23ca68b8ed0c7a30acee6c8a42ba325c428941b2a8ff2c8c514b7e26a023bcd41394a6bcb5c49015b921bd53e9e423428dae4c8117b3f3c71b057371db8f4fbe75eb5dbf73f5bd04b12308ddb76dd6b3946eb5d21e4c6f61cdb788c10785e5
8ba5d9da7727889c27784e1371b7b58d0e227270653f31cc8f273acdb9e749d5fae06c5dbf6e236cac290f9ff2b80001e361dd2e666bf94b5a928b9a4e66d5e1b85276360079fe838a88b1bc216e384d3ea9d8eb35376b0a7fb9bdc013e6f6a46395802ae873a79471facedf8b7ac563c404bb1e1c13ec7cad72d6dda53b8ccc
334ce33ebff4907d63259e9b4b57c6e77c8e73decbbe06ebbacdd4f370dd378dffa2dd4fea91f6ba2670c85ed09d30ef75c11127986e970a6a47e3b0768ee1deffdfa1565186856c15a62cd14f13f030ef2605b732da682079ea1c674c7e0fd1fc3fe0b38249ed99769596c87fa03c0976505da33f6482b00e54292da385f4ad
56eabe9ebbf478ae71b6c217e708ca6bf67c29aa22b4c886c8622c325a850decbb27090e1c243a22703628a821a6bb0e4c556f8049764588b2ad01482a73653e112ab2dc681dd21f4411e61fbd85da74d763af8b40e41d3b129f02a3a41c8fd09d68c6831c78e37ba36bcf91f34808dd1c1ce1ba8e6af5ba3a33c92297147c02
720995a017f6ff64a1c490521000c86934718d01f263a73199203694f6237cb70550ca0941494ece9f849889b21d11224634ac1c7af447ad90ab1629053341d3592a10ae16cbd707171d9cab873139d03743e27c533332586e41c8b333966bd261232d923d0048f8ff35185cbf7e4db63040c61888c4fcc444e5b540b614c24c
cbb4a237720a76cee86d358dd4ac5bcc6400cdd960a3e76cfb39c56a5c15897061e7515b754c8dac82876c480b125811c82c4f453716bb68815b99c56294ca199eaa6241e3050804ddd1d2164e95400113e7c69245730e949bd159a265425c5172b4d58b9c509dc46e8fc6ba4740e483b48ec7aa9f950a5653e674a22b751aa3
d442b1472799208d53b51554e920bfd702035c75128b02aa295b2a156e4dad0234d582b1265efd2458f118ac8f185155296a36ae94b29552dab5236023dc9b249eb0d798eb40d1c0e06f13aa765412da2a6799fe30e0a0acd767eb59163a013ce6b99b206b05696aad2e816d74ddd08b3b5e593b12626835d2da481ef4237a58
b28706cb8128413c446d5f219662a71a0650489ac58fafab82e4aeeb4b60eac1d37b10dec445f9b4b156d9bab0ab99a12e187ec1d429e4b255baf0d59ae4c86ded9c27208811a7351f765502739c43c9588e1bbf5faab52dab1605aade580d41ae989db81705d09c6b2d0c2a4467adf6e01559453a08efe5e6894fc6f435c770
6e97c298596541c6ae3a04566abd7f29f8166d5059097ff0090e72aed6302e738ee4158db7ade64a9a4498e36fb13d6051f85d6b51ab078f565388ac101e42d8995b46315c50c3363724d49a8f0c915534c7191b031c88f26bebc50b3e46829d46dbad4dce9cca8c2fdb302df71a3827049c59596847a24cd94eadd5d5b853e5
9c28ace85a62d4ed994a610d1cb5a6e2e3f1a013e23c479f1a7396f19c6a84a5709ae8b26707acc16e62e75809db39cabdad150e44937690a0ccdf8a3241581580141b00c01a0380787500e520ca345d1fa40bd1742c7a51696933d354d0c699d07a0b436278c45680a8340380740f01f02c05f193ded5aaed902cf636a1b32b
bb5479b340ebbce1520acebf0580b41702fd8a01b6f6dfdc001d5e6c9b50ce98ea1b538c15e84aecda23f2645ab1804f5f83c07a0f81f87502e0dc1c6fbdf8058570af02fb8d1a5033c1b317ad67aa4e9d88b1344490743ef1d7fa93536f8df5bf37d81617c2ff80d2fe09b2d9db4c399b41c7ce35a771701e4fd79b58ac6bfd
83b0f62826e61cc7990277298554ede56fb68efa9aab0b80efde70e6c56f6c6dadb9b18b7400d11790b8f39e41acd63deec077b8b47412b20941a6f4dec1d413992ea5062d8270c1edf4c3e2e6abd7d8960fbb49c7b45c1bd5d9bb776fecf0c33aef0a4bcff3a92e6f893630f77ef9ddd6741941bd74f0f83c297b2b8dd5f09e
249076dec1e23c578f2d048fc8793f29e57cb797f31e67cd79bf39e77cf79ff41e87d17a3f49e97d37a7f51ea7d57abf59eb7d77aff61ec7d97b3f69ed7db7b7f71ee7dd7bbf79ef7df7bff81f07e17c3f89f17e37c7f91f27e57cbf99f37e77cffa1f47e97d3fa9f57eb7d7fb0498010140209309a4e27a8142a200426150b8
64361d0f8844625138a4562a5028970ba5e2fb6c944b8b4864523924962a62781b0da6e37bacb45b934c665339a42d6cb75c2e564b35a2d66b3fa050622c663b2192c261b118b42a65369d4fa8546a553aa556ad57ac566b55bae576bd5fb0586c563b2596cd67b45a6d56bb65b6dd6fb85c6e573ba5d6ed77bc5e6f57bbe5f6
fd7fc06070583c26170d87c4627158bc66371d8fc86472593ca6572d97cc667359bce6773d9fd068745a3d26974da7d46a755abd66b75dafd86c765b3da6d76db7dc6e775bbde6f77dbfe070785c3e27178dc7e472795cbe67379dcfe8747a5d3ea757add7ec767b5dbee777bddff0786200400011090c00112a0c0001480d00
01360e0001420e00014e0e0001330f001123100001481100014d1100114f110001411200013b13001154130031571300015e1300014814000149140001591400014e1500013816000142160001471600015c160001581700015c1800315e1900013a1a0001591a00110a1d00110e2000010b2300010d2400010a260001402600
010d27000143270001482700013e2800010a2a0001102b0001432b0001452f003120310001493100310933003124330001183400310a3600011436000115370001193700011238003109390001223f0001273f0001344100112f420001334500300c4700056d47003255490033734900056d4b00000000000000000000000000
__sfx__
000100002b52329543265532555323551215511f5511c5511955118551165511455113541105410d5310b52108521075210551103511025110151102400023000130003400024000140001400024000240001400
0002000006743097530a7530975303743017210060102600006000060000600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000400000c343236450933520621063311b6210432116611023210f611013110a6110361104600036000260001600016000460003600026000160001600016000160004600036000260001600016000160001600
001000000835307343053330332301313063030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0003000032554305442e5402c5402a5302853025530225301f5301d5201a52018520155201452012520115200f5200d5200c5200c5200a5200952007520065200552004520025250151500515025000150000000
0005000011433084530e43317705127000e7000770004700007001470011700117000f7000d7000c7000b700097000870006700047000270000700017000070005700097000b7000b70009700007000070000700
00050000080300d0500b0300704500000010000770004700007001470011700117000f7000d7000c7000b700097000870006700047000270000700017000070005700097000b7000b70009700007000070000700
