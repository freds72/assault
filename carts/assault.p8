pico-8 cartridge // http://www.pico-8.com
version 19
__lua__
-- assault
-- by @freds72

#include includes/bold.lua

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

-- return 
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
-- sprite helpers
-- convert a sprite number to a sprite stucture
function make_sprite(s,sw,sh)
	sw=sw or 8
	sh=sh or 8
	return {spr=s,ssx=band(s*8,127),ssy=8*flr(s/16),sw=sw,sh=sh,sx=-sw/2,sy=-sh/2}
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
			if(sget(sx+i,sy+j)!=tc) bits=bor(bits,lshr(0x8000,i))  
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
local _npcs,plyr={}
local _map,_cells,_cells_map,_grid,_map_lru

-- todo: remove for optimisation
local gravity=-0.04

-- player factory
function make_plyr(x,y,z,angle)
	local reload_ttl,reload_nuke_ttl,acc,da,dz,mortar_angle,underwater=0,0,0,0,0,0
	-- threads positions & velocities
	local lthread,rthread,lthread_acc,rthread_acc=0,0,0,0
	local states,state,nuke_mode,driving_mode
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

	local dust_sprites={212,213,214}

	function fire_bullet(ca,sa)
		if btn(5) and reload_ttl<0 then
			make_bullet(bullet_cls,x,y,0,-sa/2,-ca/2)
			reload_ttl=10
		end
	end

	function fire_nuke(scale)
		if btn(5) and reload_nuke_ttl<0 then
			-- polar coords
			local cm,sm=scale*nuke_v*cos(mortar_angle),-scale*nuke_v*sin(mortar_angle)
			local ca,sa=cm*cos(angle),cm*sin(angle)
			make_part(nuke_shell_cls,x,y,z,-sa,-ca,sm)
			-- next nuke
			reload_nuke_ttl=15

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
			sprite,z,dz,driving_mode,nuke_mode=default_sprite,0,0,true
			-- if(not trails) make_part({sw=2.5,dsw=-0.1,ttl=6000,c=6,kind=6,trail={}},58,120) make_part({sw=2.5,dsw=-0.1,ttl=6000,c=6,kind=6,trail={}},68,120)
		
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
				if(btn(2)) acc+=0.05 turn_scale=0.7 lthread_acc=1 rthread_acc=1
				if(btn(3)) acc-=0.05 turn_scale=0.7 lthread_acc=-1 rthread_acc=-1
				
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
				local area=bor(xarea,yarea)
				underwater=nil
				if band(area,0x2)>0 and abs(acc)>0.1 then
					cam_shake()
				-- jumppad
				elseif band(area,0x8)!=0 then
					underwater=true
				elseif band(area,0x4)>0 then
					local i=bor(flr(x),shl(flr(y),7))
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
						
				fire_nuke(1)

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

				if(btn(3)) mortar_angle+=0.01				
				mortar_angle=mid(mortar_angle*0.95,0,0.2)				
				sprite=lerpa(mortar_sprites,mortar_angle/0.2)

				fire_nuke(1)
			end
		end,
		drop=function()
			local bounces=2
			driving_mode=nil
			return function()
				z+=dz
				dz+=gravity
				if z<0 then
					bounces-=1
					z=0
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
			if driving_mode then
				if(underwater) pal(5,6)
				if lthread_acc>0 then
					spr(dust_sprites[flr(time_t/4)%3+1],64,119,1,1,false,true)
				elseif lthread_acc<0 then
					spr(dust_sprites[flr(time_t/4)%3+1],64,104)
				end
				if rthread_acc>0 then
					spr(dust_sprites[flr(time_t/4+1)%3+1],52,119,1,1,true,true)
				elseif rthread_acc<0 then
					spr(dust_sprites[flr(time_t/4+1)%3+1],52,104,1,1,true)
				end
				pal()
			end
			-- player
			if(underwater) pal(1,3) pal(9,11) pal(4,11)
			spr(sprite.spr,self.sx,self.sy,2,2)
			
			if driving_mode then
				sspr(40+2*flr((lthread%3+3)%3),32,2,7,58,111)
				sspr(40+2*flr((rthread%3+3)%3),32,2,7,64,111,2,7,true)
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
		hit=function()
			-- make_blast(x,y)
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

-- particles: do not interact with actors or background
-- bullets: interact with actors or background + can spawn particles
local _parts,_bullets={},{}

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
				p.sx=64+shl(ca*x-sa*y,3)-p.sw/2
				p.sy=112+shl(sa*x+ca*y,3)-p.sh/2

				local col,a,b,x0,y0,y1=collide(p,plyr)
				if col and intersect_bitmasks(a,b,x0,y0,y1) then
					--local ax,ay=flr(a.sx),flr(a.sy)
					--p.rect={ax+x0,ay+y0,ax+a.sw-1,ay+y1-1}
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
			if solid(x0,y1) or solid(x1,y0) then
				parts[i]=nil
				-- effect
				make_part({sw=2,c=7,ttl=2+rnd(2),kind=5},x1,y1,1)
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
		elseif p.kind==6 then
			local trail=p.trail
			if(time_t%4==0) add(trail,{x=p.x+1-rnd(2),y=p.y,sw=p.sw})
			for k=#trail,1,-1 do
				local pt=trail[k]
				pt.y+=0.4
				pt.sw+=p.dsw
				if(pt.sw<0) del(trail,pt)
			end
			--p.dr*=p.ddr
			-- if(p.sw<0) parts[i]=nil
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
			local dx,dy=64+shl(ca*x-sa*y,3),112+shl(sa*x+ca*y,3)
			if p.draw then
				p:draw(dx-w/2,dy-h/2,z)
			elseif p.kind==3 then
				local sx,sy,w=48,47,17
				if(time_t%8<4) sx,sy,w=61,32,13
				sspr(sx,sy,w,w,dx-w/2,dy-w/2)
			elseif p.kind==5 then
				-- bullet impact
				circfill(dx,dy,w,p.c)
			elseif p.kind==6 then
				--fillp(0xa5a5)
				for _,pt in pairs(p.trail) do
					circfill(pt.x,pt.y,pt.sw+1,0x6d)			
				end
				for _,pt in pairs(p.trail) do
					circfill(pt.x,pt.y,pt.sw,0x66)			
				end
				--fillp()
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

local blast_cls={
	ttl=10,
	draw=function(self,x,y,w)
		palt(0,false)
		palt(14,true)
		local cc=lerpa(blast_circles,self.t/self.ttl)
		for i=1,#cc do
			local c=cc[i]
			if(c.fp) fillp(c.fp)
			(c.fn or circfill)(x,y,c.r*w,c.c)
			fillp()
		end
		palt()
	end,
	update=function(self)
		-- todo: blast crater
		-- if(self.t==5) add(blasts,{x=x,y=y})
		return self
	end
}
function make_blast(x,y)
	-- explosion part(s)
	return make_part(blast_cls,x,y)
end

-- nuke "particle"
function make_nuke(x,y)
	local r0,r1,fp,t=8,8,#dither_pat,0
	-- nuke always at floor level
	return make_part({
		ttl=27,
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
					local x0,x1=max(rr0-r*r)^0.5,(rr1-r*r)^0.5
					rectfill(-x0,r,-x1,r)
					rectfill(x0,r,x1,r)
					rectfill(-x0,-r,-x1,-r)
					rectfill(x0,-r,x1,-r)    
				end
				circ(0,0,r0,12)
				fillp()
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
			-- 0: empty tile
			fn(word[i],x,y)
			x+=1
			if(x>127) x=0 y+=1 
		end
	end
	return mem
end

-->8
-- npc functions
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
	-- note: useless for 'map' npc
	local s=base.sprite or base
	local w,h=s.sw/16,s.sh/16
	-- x---->x
	--    0  |
	-- x<----x
	local quad={
		{x=0,y=0,ix=-w,iy=-h},
		{x=0,y=0,ix=w,iy=-h},
		{x=0,y=0,ix=w,iy=h},
		{x=0,y=0,ix=-w,iy=h}
	}

	return add(_npcs,setmetatable(base,{
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
				local ix,iy=g.ix,g.iy
				-- translate to cam space
				ix,iy=scale*(ca*ix-sa*iy)+x1,scale*(sa*ix+ca*iy)+y1
				local code=0
				if ix>14 then code=2
				elseif ix<2 then code=1 end
				if iy>16 then code+=8
				elseif iy<0 then code+=4 end
				outcode=band(outcode,code)
				-- to screen space
				g.x=8*ix
				g.y=8*iy
			end
			-- visible?
			if outcode==0 then
				if(hit_t>0) memset(0x5f01,0x7,15) palt(0,true)
				tquad(quad,self.uv)
				pal()
				--print(self.z,8*x1+8,8*y1,2)
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
			make_blast(self.x,self.y)
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
			angle=self:control()
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
	local acc,move_t=0.1,0
	local update_path=cocreate(update_path_async)
	local lerp_angle=make_lerp_angle(0,0.1)
	local dx,dy=0,0
	-- can npc move?
	local id=npc_id
	npc_id+=1

	local tank={
		w=0.8,
		h=0.8,
		sprite=light_tank_sprite,
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
			local angle=0
			if move_t<0 and #self.path>0 then
				-- get result from a*
				local input=self.input
				if not input or dist(self.x,self.y,input.x,input.y)<1 then
					input=pop(self.path)
					self.input=input
				end
				if input then
					angle=lerp_angle(input.x,input.y,self.x,self.y)

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
				self.x+=dx
			end
			if band(yarea,0x1)==0 then
				self.y+=dy
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

	return make_npc(tank,x,y)
end

local heavy_tank_sprite=with_hitmask(make_sprite(182,32,16))
function make_heavy_tank(x,y)
	local angle,acc,move_t=0,0.1,0
	local update_path=cocreate(update_path_async)
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
		-- co-routine data
		seek_dly=60,
		path={},
		control=function(self)
			return 0.1
		end
	}

	return make_npc(tank,x,y)
end

local msl_tank_sprite=with_hitmask(make_sprite(178,32,16))
function make_msl_tank(x,y)
	local angle,acc,move_t,reload_ttl=0,0.1,0,0
	local update_path=cocreate(update_path_async)
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
						make_homing_msl(self.x,self.y,0)
					end
				end)
			end
			return 0
		end
	}

	return make_npc(tank,x,y)
end

local heavy_turret_sprite=with_hitmask(make_sprite(76,32,24))
function make_heavy_turret(x,y)
	local reload_ttl,toward=0,make_lerp_angle(0,0.1)
	local turret={
		uv={
			2,0,
			6,0,
			6,3,
			2,3},
		hp=10,
		sprite=heavy_turret_sprite,
		control=function(self)
			-- account for turret size
			local x,y=self.x,self.y
			local angle=toward(x,y,plyr.x,plyr.y)

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
						if(self.hp<=0) return 
						make_bullet(large_bullet_cls,cx-i*sa*w,cy+i*ca*w,0,0.12*ca,0.12*sa)				
					end)
				end
			end
			return angle
		end
	}
	return make_npc(turret,x,y)
end

function make_static_turret(x,y)
	local reload_ttl=0,0
	local turret={
		sh=16,
		sw=16,
		hp=1,
		draw=function()
			-- built-in map
		end,
		die=function(self)
			self.dead=true
			local x,y=self.x,self.y
			make_blast(x+1,y+1)
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
						for i=0,0.75,0.25 do
							local ca,sa=cos(i),-sin(i)
							make_bullet(small_bullet_cls,x+0.5*ca+1,y+0.5*sa+1,0,0.2*ca,0.2*sa)				
						end
					end
				end)
			end
		end
	}
	return make_npc(turret,x,y)
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
		collide=function(self,p)
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
					--hidden=true
				end)
			end
			return 0
		end
	}
	return make_npc(turret,x,y)
end

-- missile
function make_msl(x,y)
	-- get direction
	local ttl,angle=0,atan2(plyr.x-x+0.5,-plyr.y+y+0.5)
	local dx,dy,dz=0.2*cos(angle),-0.2*sin(angle),0.4

	local msl={
		sw=8,
		sh=8,
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
			self.x+=dx
			self.y+=dy
			self.z+=dz
			if(ttl<0 or self.z<0) self:die() return
			dz+=gravity
			return angle 
		end
	}
	return make_npc(msl,x,y)
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
			6,0,
			7,0,
			7,1,
			6,1},
		collide=function() end,
		control=function(self) 
			ttl-=1
			if(ttl<0) self:die() return
			
			angle_ttl-=1
			-- homing mode?
			if angle_ttl<0 then 
				angle=toward(self.x+0.5,self.y+0.5,plyr.x,plyr.y)
			end

			local dx,dy=(0.2+acc)*cos(angle),-(0.2+acc)*sin(angle)
			acc*=0.87
			local xarea,yarea=get_area(self,dx,0),get_area(self,0,dy)
			-- solid?
			if bor(band(xarea,0x1),band(yarea,0x1))!=0 then
				self:die()
			else
				self.x+=dx
				self.y+=dy
			end
			return angle 
		end
	}
	return make_npc(msl,x,y)
end

-->8
-- map & draw helpers
#include includes/tquad.lua

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

function map_set(i,j,s)
	-- no need to track 'static' tiles
	if(fget(s)!=0) _map[i+128*j]=s
	-- cell coord (128x128)->(4x4)
	local ck=flr(i/32)+4*flr(j/32)
	local cell_map=_cells_map[ck]
	-- cell is 4*dword with a stride of 128/4 = 32
	-- cell entry is packed as a dword
	local k=4*(flr(band(i,31)/4)+32*band(j,31))
	-- shift 
	local shift=8*(i%4)
	cell_map[k]=bor(
		band(cell_map[k] or 0,rotl(0xffff.ff00,shift)),
		shl(0x0.0001,shift)*s)

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
		local ix,iy=32*(i%5)-x,32*flr(i/5)-y
		ix,iy=scale*(ca*ix+sa*iy)+8,scale*(-sa*ix+ca*iy)+14
		local outcode=0
		if ix>14 then outcode=2
		elseif ix<2 then outcode=1 end
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
		-- visible or partially visible?
		if band(
			band(cell[1].outcode,
	   		band(cell[2].outcode,cell[3].outcode)),
			   cell[4].outcode)==0 then
			viz[k]=cell
		end
	end
	
	-- draw existing cache entries
	for i,entry in pairs(_map_lru) do
		local cell=viz[entry.k]
		if cell then
			local offset=32*i
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
		local mem=0x2000+mini*32
		for base,v in pairs(_cells_map[k]) do
			poke4(mem+base,v)
		end
		-- draw with fresh cache entry		
		local offset=32*mini
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
end

-->8
-- init/update/draw/states
function play_state()
	local lives=3
	-- reset
	_npcs,_parts,_bullets,plyr={},{},{},make_plyr(41,54,8,0)

	-- init map
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
			jumppads[i+128*j]=3
		end
		-- update map
		map_set(i,j,s)
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

	-- todo: load actors from data
		
	make_heavy_tank(33,57)
	make_msl_tank(33,52)
	make_heavy_turret(23,35)
	make_heavy_turret(30,35)
	make_static_turret(23,42)
	make_static_turret(28,42)
	make_static_turret(23,48)
	make_static_turret(28,48)
	make_msl_silo(28,58)

	local red_blink={0,1,2,2,8,8,8,2,2,1}

	return {
		draw=function() 
			local px,py,pz,pangle=plyr:get_pos()

			cls()
			clip(16,0,128-32,128)
			-- blinking lights
			pal(8,red_blink[flr((5.3*time())%#red_blink)+1]) 
			draw_map(px,py,pz,pangle)
			pal()
		
			-- draw
			for i=1,#_npcs do
				_npcs[i]:draw(px,py,pz,pangle)
			end
		
			plyr:draw()
		
			draw_parts(_bullets,px,py,pz,pangle)
			draw_parts(_parts,px,py,pz,pangle)
			
			--line(64,64,64+16*ca,64+16*sa,11)
			--line(64,64,64-16*sa,64+16*ca,8)
		
			-- swamp green
			pal(11,138,1)
		
			printb("SCORE",19,1,1)
			printb("SCORE",18,0,14)
		
			local s=tostr(flr(time()*256))
			printb(s,18,7,0)
			printb(s,18,6,7)
			
			printb("HI",101,1,1)
			printb("HI",100,0,14)
		
			printb("99999",83,7,0)
			printb("99999",82,6,7)
		
		
			printb("TIME",55,1,1)
			printb("TIME",54,0,14)
		
			local t=tostr(99-flr(time()))
			printb(t,61,7,0)
			printb(t,60,6,7)
		
			for i=0,lives-1 do
				spr(166,18+i*6,120)
			end		
		end,
		update=function()
			cam_update()

			plyr:update()
		
			-- update actors
			for i=#_npcs,1,-1 do
				local npc=_npcs[i]
				npc:update()
				if(npc.dead) del(_npcs,npc)
			end
		
			update_parts(_bullets)
			update_parts(_parts)
		end
	}
end

local _state
function _init()
	
	--add(_npcs,make_tank(33,60))
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

	_state:draw()
	--rectfill(0,0,127,8,1)
	--print(stat(1).."/"..stat(7).."/"..stat(9).." "..stat(0).."kb",18,2,7)
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
dddddd7d5633335333336733ddd77ddd1111111111dd6dddd1111111da0ddddddd101ddd3333333333333333666666661555499a4551221221221554a9945571
dddddddd3533333333535d33dddddddd111111111d6dd5dd11111111daaddddddddddddd3333333333333333666666661111499a4112442442442114a9941111
0000000000000000000000000066000000000000d555d50000007777700000000888880000000000333333333333333300000000000000076d50000000000000
0000000000000000002222000d55d00000000000d56d6d00000070007000000000888000000000003330330030303333000000000000067766d5500000000000
0000000000000000022ee220d511ddddddd000006d6dd500000000000000000000080000000000003333308500333333000000000000677766dd550000000000
000245d5d5d9000002e77e2065dd6666666d00006dd56d0000000000000000000000000000000000330330528003303300000000007777766666666d00000000
00184555554da00002e77e207d77f777777f0000d56d6d007700000000077800000800000800000033330420140003330000000007666667777666d500000000
0012244996549000022ee220fffffffffff000006d6dd5007000000000007880008080008800000033304d6001004033000000006766666666677d57799a7700
0012499ffff200000022220000660000000000006dd56d0070000070000078880800080888000000303085d8024da03300000006776666666666656664446600
001292d9af616d000000000000660000000000000000000070000000000078800080800088000000330880820285d00300000007776666666666655111111100
001292d9af75770000000000006600000000000000000000770000000007780000080000080000003304d0000028003300000067776666666666655500000000
0012499ffff2000000000000005500000000000000000000000000000000000000000000000000003005a808d00282130000007667666666666665d7799a7700
001224499654900000099000d511ddddddd0000000000000000000000000000000080000000000003301400594000133000000d6676666666666656664446600
00184555554da000009a790065dd6666666d0000000000000000700070000000008880000000000033331002502803330000005d676666666666655111111100
000245d5d5d90000019aa9007d77f777777f00000000000000007777700000000888880000000000333330080082333300000005d76666666666655500000000
000000000000000001199000fffffffffff00000000000000000000000000000000000000000000033033100000303330000000557666666666665d7799a7700
0000000000000000001100000666600000000000000000000000000000000000000000000000000033333301303333330000000057666666666dd56664446600
000000000000000000000000006600000000000000000000000000888880000000000000000000003333333333333333000000000766dddddddddd5111111100
0000000000000000000000000000000000000000000000000000000888000000000000003366677667766776677666330000000000ddddddddddddd500000000
0000000000000000000000000000000000000000000000000000000080000000000000003676d776d776d776d77667630000000000005dd55551110000000000
0000000d6000000000056000000000000000000000000000000000000000000000000000d7711111111111111111177600000000000005dd5511100000000000
0000090d709000000056760000000000000000000000000000000000000000000000000016d11dd11dd11dd11ddd11d6000000000000000d5110000000000000
00009a9d69a900000056760000000000000000000000000000000000000000000000000066ddddddddddd4adddddd16600000000000000000000000000000000
0000d5f5df5d00000015650000000000000000000000000080000088888000008000000077dddddddddd149dddddd17700000000000000000000000000000000
0000d596795d00000056760000000000000000000000000088000080008000088000000077dddddddddd11dddddd117700000000000000000000000000000000
00006df88fd6000000000000000000000000000000000000888000800080008880000000d6dddddddddddddddddd11d600000000000000000000000000000000
00056df5dfd6500000000000dddddddddddddddddddddddd88000080008000088000000066ddddddddddddddddddd16600000000000000000000000000000000
000fd596795df00000000000d777777dddddddddd7dddddd80000088888000008000000077ddddddddddddddddddd17700000000000000000000000000000000
000f6d4664d6f000000cc000dd7dd7ddddddddddd77ddddd00000000000000000000000077dddddddddddddddddd117700000000000000000000000000000000
0009d549f95d900000c7cc00ddd77dddddddddddd7d7dddd000000000000000000000000d6dddddddddddddddddd11d600000000000000000000000000000000
00049f1491f9400001cc7c00ddddddddddd77dddd7d7dddd00000000000000000000000066ddddddddddddddddddd16600000000000000000000000000000000
0001481111841000011cc000dddddddddd7dd7ddd77ddddd000000008000000000000000677dddddddddddddddddd77600000000000000000000000000000000
000011000011000000110000ddddddddd777777dd7dddddd0000000888000000000000001d76677667766776677667d100000000000000000000000000000000
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
0070000000d70000007000003333333333b5bb3335b5bbb30060000000000000113333333333331133333311333333111111111111111113d56ddd555555576d
0d600000006600000d600000333bb33335577bb351577bbb0d9d00000000000033333333333333113333531133333333333333333333333316d0000000000576
077760000977777009777d0033566b3335d667b315d887bb9d2d9000000000003333333333333311333333113333333353333333533335331600000000000057
9fff7f009afffff69a9ff600335d6b3335d667b311d887bb9d4d9000000000003333333333333311333333113333333333533333333333331600000000000057
066660000966666009666d0033155333315ddbb3151ddbbb19191000000000003333333333333311333533113333333333333333333333331600000000000057
0d600000006600000d600000333333333315b5331151b5b301010000000000003333333333333311333333113333333333333533533333331600000000000057
0070000000d7000000700000333333333333333311151b3300000000000000003333333333333311333333113333333333333333333333331600000000000056
33333333333333330000111010111110011110111ddd100011222222442002221122410000000000333333333333333300000000000000001600000000000056
3333315555dd533300002865656d661005667d6667692000128999997a951999a499a91000000000333333333333333300000000000000001600000000000056
33331555d567d533000028666dd5d61005667d66676920001289a9a97a944999a499a991000000003333333335b63333000000000000000016000000000000d6
3331515656667d5300005d66dd55550001667d6677fd1000154a7a7777999999a499a9920000000033333d6331d63333000000000000000016000000000000d6
33155567d5d667d3000015d655111111551d65df6d1000000151d5d5d499911151559441000000003333db7633d3333300000000000000001600000000000066
33155dd65555ddd300000566dd15555706567d67661000000151d5d5d49912444442411000000000333db3b634233333000000000000000017d0000000000661
315555611155d75300000566dd12555760567d67661000000151d5d5d4912499999415d1000000003331db32343333330000000000000000317666666666661d
155156111115565300000566dd15555706567d67661000000155544999924999999ad666000000003333124433333333000000000000000033111111111111dd
1151d6d011d6665300000566dd15555760567d6766100000015554499994a999999a677700000000333333333433333333333333333333566633444333333333
11556ddddd66675300000566dd12555706567d67661000000151d5d5d49aa7999997566d00000000333335643425633333333333333335dddd63344333433333
1151d76d66667653000005666d15555760567d676d1000000151d5d5d499aa77777745100000000033335db63353b6333333333333435dd67dd6333344443333
1115556d666d6653000015d6ddddddd1551d65dfd510000001515151529999aaaaa99d4100000000333313d633155333333443344435dd5d6ddd633344433333
311551551666d53300005d666667661005667d66676d100015442424aa999999a499a99200000000333331133333333333344333445ddd55ddddd63333333333
3111151557d66533000028666667661005667d6667692000128949497a9aa999a499a9910000000033333333333333333334344335dddddddddddd6333433333
331111111dd65333000028656567661005667d6667692000128999997a945999a499a910000000003333333333333333344433335dddddddddddddd633444433
333111110d6533330000116161666d100d66d16666dd10001124446666410444d144410000000000333333333333333334433335dddddddddddddddd63344433
000000003333dddddddddddddddd333300000000000000000000000000000000000000000000000000000000000000003333435ddd8dddddddddd8ddd6334443
0000000033dd5555555555555555dd330000000000000000000550000000000000000000000000000000000000000000344335ddd88dddddddddd88ddd633443
001100003d55666666666666666655d3000000000000000000005000000000000000000000000000000000000000000034435ddd888dddddddddd888ddd63333
001100003d666b6b6b66b6b6b66665d300000000005500000000005000000000000000000000000000000000000000003335dddddddddd8dd8dddddddddd6333
00000100d66b6b66bbb6bb66bb6b665d0550000005555000005500500000000000000000000000000000000000000000335dddddddddd88d688dddddddddd633
00001100d6b6bbbbbbbbbbbbbbb6b65d5555000000055000005500000000090d6090000000000000000000000000000035dddddddddd888d6888dddddddddd63
01000000d6bbbbbbbbbbbbbbbbb6b65d5555000000050000000000000000969289690000000000d88d000000000000005dddddddddd8888678888dddddddddd6
00000000d66bbbbbbbbbbbbbbbbb665d0550000000000000000000000000d595d95d000000006d95d9d60000000000001dd67ddddddddd688766dddddddd67d6
00000000d6b6bbbbbbbbbbbbbbbbb65ddddddddd79977997dddddddd000fd546645df0000004989679899000000000001d5d6ddddddd55d886ddddddddd5d6d6
00000000d66bbbbbbbbbbbbbbbb6665ddddddddd77997799dddddddd00096d49f9d69000000542249224d000000000001d55ddddddd8888d68888dddddd55dd5
00000000d6bbbbbbbbbbbbbbbbbb665ddddddddddddddddddddddddd00049f0490f9400000005502205500000000000031dddddddddd8885d888dddddddddd53
00000000d6bbbbbbbbbbbbbbbbbbb65dddddddddddddddddd777777d0005480000845000000000000000000000000000331dddddddddd885d88dddddddddd533
00000000d666bbbbbbbbbbbbbbbb665dddddddddddddddddd777777d00005500005500000000000000000000000000003331dddddddddd8dd8dddddddddd5333
00000000d6b6bbbbbbbbbbbbbbb6b65ddddddddddddddddddddddddd000000000000000000000000000000000000000033331ddd888dddddddddd888ddd53333
00000000d66bbbbbbbbbbbbbbbb6665d99779977dddddddddddddddd0000000000000000000000000000000000000000334331ddd88dddddddddd88ddd533443
00000000d6bbbbbbbbbbbbbbbbbbb65d79977997dddddddddddddddd00000000000000000000000000000000000000003443331ddd8dddddddddd8ddd5334344
00000000d6bbbbbbbbbbbbbbbbbb665ddddddd9777dddddd00000000000000000000000000000000000000000000000034443331dddddddddddddddd53444444
00000000d666bbbbbbbbbbbbbbb6665ddddddd9997dddddd000000000000000000000000000000000000000000000000334333331dddddddddddddd533443433
04044400d66bbbbbbbbbbbbbbbbbb65ddddddd7999dddddd0000000000000000000000000000000000000000000000003333333331dddddddddddd5334433333
0f4fff40d6bbbbbbbbbbbbbbbbbb665ddddddd7779dddddd00000000000000000000000000000000000000000000000033334433331dddddddddd53334433333
072777f03d6bbbb6bb66bbb66bb665d3dddddd9777dddddd000000000000000000000000000000000000000000000000333344333331ddd67ddd533344433333
0f0fff003d66bb6b6b6b6bb6b66655d3dddddd9997dddddd0000000000000000000000000000000000000000000000003333333333331d5d6dd5344333333333
0000000033dd6666666666666666dd33dddddd7999dddddd0000000000000000000000000000000000000000000000003333333333333155dd53444333333333
000000003333dddddddddddddddd3333dddddd7779dddddd00000000000000000000000000000000000000000000000033333333333333111533333333333333
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
0000010101000000000000000000000000000100010000000000000000000000000001010100000001000000000000000002020000000000000000000000000000000000000000000000020200000000000000000000000000000202000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000001010000000000000000000000000000010100000000000000000002000000000000000000000101000000000000000000000000000001010000000000000000000000000000000000000000000000000000000000000000080000000000000000000000040000000000000000000000000000000000
__map__
5c0600804060503824160d0784426150b864361d0f8844625138a4562d178c466351b8e4763d1f904864523924964d27944a6552b964b65d2f984c665339a4d66d379c4e656d09e4f67d3f9f4ea773c0100e8d47a45268e046850a4ed0a2d1982acaa556ad57aa2a8294da7495a012aa806c40109d96cd67b3d6ab95d91d7eca
01475c51f64b28140d77bc0180e13b55b2496eb827b049fba04c6e38c46202c160bdf2b77eb684827704723f2b85c3e24718bc6df72121c02393c9fd1e63359bc663ad79f8f6872a8ec284c4a26da6d44c27d56b2416ec9e96d1bfb2ac04623126af751bde64f63c0b356e79c78fb40442358d8fadd6e170f87c5e8682bf76bc
de40e129ff77221519fa7d5ea0bf93cd26af85717f3fa7b78defdd84be5f4fafbbf1e77f1fd7ddff475f1805f37da044895f1d18b8360e8381282a0b0480c0340e03c750583685a187fa136b4120701d0781f62c3488e2587e204715f0b02d0b82f89e2f8c62b8b11a57c3c0f43e0fe1a0d23a8f2368dd1857c349183589e460
d03590a4345a0681d8c9350a501cf9380057db36d9b609e5341d50528035325580dc757dd775caa9790498153560c098662995ac99dcc0066a9ce6c048ad59810045662ad58550c0531f86f27e9f67f9e10d72413080215a1712396963def6f28f59a98a2d0ca35660049f2389fa889ea8566a12795b29d5a0aba9d4041aaa00
48f27c8f23da3a85825959e74150305cc59e7052a8540dbc330cd338cf5949ead6a4ad6b427d65a9ebb00abdafa80a0aad4fd9204cdc374de37d675c1a4b309e5d2d16eaaab55c0b0149b50ec3b4ee3bed56f9b9b9eda59688bde7f64c310cafdbf0310c413a05579f1653f0fd3f8ffba563b42c267e8da6165a6001c02fdc58
32c02d575dc176b1d2c56fae66b4e822c7f1ac5effc571ac30132c1c5502f6c86a84e5d2c9676c0301bf318bf6be759652a642a769bb9f24c683158813ce6fc9db1bae901d072250ae870164c634ad4f4ca5502d3f32991414bb44ba56fc5f0b5d34d4035b426609c6724bb526fd64ca5cc0a42a6176696330c0ae6416bca095
5de92ab4f61c8314c0413344d2e20d3594390eb47bd35abd985dff4ee06e9e4df0de364598d138b9d388e3a7a947ddbce397c93795d86ad4a7a8d93a53879ee7dca04f73ae759d3adacaf76dbaea9c93d5ff99cf32b04fafe7ba0d1f8ced7a3ee38eab30ed9f30ed167f45652b2eb98d22eb32af0bc4e77a0703baf2fa5dffac
f2167f96d6a0fbd747c0d2f3d04cf13cbf13c8f3733e0a7ba504e84edf82b56819c3ce470fb1489734d0ff0e10246f6e45e10133c85116a3fc7f0e8910b61544a80cb801594b915f1d93b90257016472451e07b8281704902b6057c688c12b456868cb99bf83895957c28394f30aa3058490961342766a7015942d2e2bccb416
a664f3db797085ea8d511cb2d0fb8b33d36ec8e0e99c387a598d1c4a898764e210e560a81522a1542a89594117ee72df3c513904f229c555d2cb5df10f8686f61618356cacd6544c380f4de49250351aa361cc8dc44d9a44d307106151a4896e09f3c7b29e74e3fb2c3b50764148e908608c1c4b8c8ff1c7c8d8a859a03b5e49
f25232b1b2cf168ed48f2cd1a0bf8d00287064922d8e2b56372d95d32a892c5395f09d8ecbb9792e4e94bc97b11505c323bcd76502c398d1bd2bccb999336674cf9a0d4664cd34c9345b39516d73666d40099857dbecdf9c055a5b2372dc780f0ce79d05e9efbb699a5b80a8e69e13c6794f39e839a7317b9373b4c91fb40e39
27f2513160542184408a6367ccdd32422845d0aa14596851e802f43285d13a0214c2a055a0d38d1616e13426e8ed1d2cb4768851fa3d496808630c81968cceca1004ccab737a343c19d111174c01550a316968dbc8c99e57cd109f718f9691533a3b5041d51d9fe8229e4ce2beb329b373a654d29b5121eb52a83ccba7c68ea3
38ca8605ea283aab9496aa99ca973e84f512aa144e88512a1d44e8952b98482a9f524ac347472d77a4948692d24ae134533ce900d58c0b0f49d3608d4d574ae99e7acf0b043d2c5d86afb34133a5b36d636ca1b4b2159684567a6b4deced0a9fc3929b08c11b6928950ab234f4c955fb5826e88546138276d8d24a3b6a6a64fb
40f546d25bbb4a236d388bb6d3e8d9597abb6c6e35b213b6d04d9b8b10939ddc4e7f9469104bf981756eb4c09733b66a5db55d35aef5dfbc1786f15e3bc9796f35e7bd17a6f55ebbd97b6f75efbe17c6f95f3be97d6fb5f7bf17e6fd12b0ff0040209309a4e27a8142a200426150b864361d0f8844625138a4562a5028970ba5
e2fb6db91690486452392458c4f0361b4dc6f75bb24b2f984c6650c5b2dd70b9592cd68b599cf67d3f88b198ec864b0986c462d02954ba65369d4fa8546a553aa556ad57ac566b55bae576bd5fb0586c563b2596cd67b45a6d56bb65b6dd6fb85c6e573ba5d6ed77bc5e6f57bbe5f6fd7fc06070583c26170d87c4627158bc66
371d8fc86472593ca6572d97cc667359bce6773d9fd068745a3d26974da7d46a755abd66b75dafd86c765b3da6d76db7dc6e775bbde6f77dbfe070785c3e27178dc7e472795cbe67379dcfe8747a5d3ea757add7ec767b5dbee777bddf8800000000000000000000000000000000000000000000000000000000000000000000
