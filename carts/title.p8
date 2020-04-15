pico-8 cartridge // http://www.pico-8.com
version 19
__lua__

#include includes/tquad.lua
#include includes/bold.lua
#include includes/bigscore.lua

-- helper functions
function lerp(a,b,t)
 return a*(1-t)+b*t
end
function lerpa(a,t)
 return a[flr(#a*t)+1]
end

local shkx,shky=0,0
function cam_shake()
	shkx,shky=0,1+rnd()
end
function cam_update()
	shkx*=-0.7-rnd(0.2)
	shky*=-0.7-rnd(0.2)
	if abs(shkx)<0.5 and abs(shky)<0.5 then
		shkx,shky=0,0
	end
	camera(shkx,shky)
end

function padding(s,n)
	n=n or 5
	return sub("00000",1,n-#s)..s
end

local dither_pat={0b1111111111111111,0b0111111111111111,0b0111111111011111,0b0101111111011111,0b0101111101011111,0b0101101101011111,0b0101101101011110,0b0101101001011110,0b0101101001011010,0b0001101001011010,0b0001101001001010,0b0000101001001010,0b0000101000001010,0b0000001000001010,0b0000001000001000,0b0000000000000000}

-->8
-- globals
local title_z,landed=-7
local hi_score,coins=0,1

-->8
-- update/draw
function _init()
	cartdata("freds72_assault")
	-- make sure player get at least 1 coin!
	coins=max(dget(0),1)
	hi_score=dget(1)
end

function _update()
	if(btnp(4) or btnp(5)) load("assault.p8")

	cam_update()

	title_z=min(title_z+0.15)
	if(title_z==0 and not landed) landed=true cam_shake()
end

function _draw()
	cls()
	clip(16,0,128-32,128)
	local bands={1,1,2,13}
	local k,dk=0,(#bands-1)/64
	for i=0,63 do
		local c0,c1=bands[flr(k)+1],bands[flr(k)+2]
		local c=bor(shl(c0,4),c1)
		local t=k%1
		fillp(lerpa(dither_pat,t))
		rectfill(0,i,127,i,c)
		k+=dk
	end

	--if(btnp(0)) n+=1
	--if(btnp(1)) n-=1
	bands={6,13,5,0}
	local n=11
	k,dk=0,(#bands-1)/n
	for i=64,64+n-1 do
		local c0,c1=bands[flr(k)+1],bands[flr(k)+2]
		local c=bor(shl(c0,4),c1)
		local t=k%1
		fillp(lerpa(dither_pat,t))
		rectfill(0,i,127,i,c)
		k+=dk
	end

	-- 3d/rotation effect
	local angle=title_z/14-0.25
	local w=8/(title_z+8)
	local ca,sa=cos(angle),-sin(angle)
	-- title vertices
	local vertices={
		{x=-48,y=40},
		{x=48,y=40},
		{x=48,y=0},
		{x=-48,y=0}
	}	
	for _,v in pairs(vertices) do
		local x,y=-sa*v.x+ca*v.y,ca*v.x+sa*v.y
		v.x=16+48+x*w	
		v.y=64-(-2-y)*w	
	end
 
	pal(14,0) 
	tquad(vertices,{0,0,12,0,12,5,0,5})
 	pal()
 
 	if landed then
		if(flr(8*time())%8<4) printb("INSERT COINS",30,80,14) printb("🅾️❎",56,87,2)

		printb("SCORE",19,1,1)
		printb("SCORE",18,0,14)

		printb("00000",18,7,0)
		printb("00000",18,6,7)
		
		printb("HI",101,1,1)
		printb("HI",100,0,14)

		local hi=padding(score_tostr(hi_score))
		printb(hi,83,7,0)
		printb(hi,82,6,7)
	end
 
	printb("FREDS72",46,120,8)
end

__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000066666666660000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000006666d000000006ffffffff60000006666000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000666ffff6d500000069f9f9f9f600000d6fff660000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000066fff949496d520000649494949600005d6499ff6000000000000000000000000000000000000000000000000000000000000000
000000000000000000000006ff9994944446d52000644444444600025d69494960d6660000000000000000000000000000000000000000000000000000000000
00000000000000000000006f4949444424246d5206424242424260025d64949465d69f6600000000000000000000000000000000000000000000000000000000
0000000000000000000000649444424242226d5206222222222260025d64444965d6949f6d660000000000000000000000000000000000000000000000000000
0000000000000000666d56444424242422226d5206212121212160025d62424465d649496d6f6000000000000000000000000000000000000000000000000000
0000000000000666f996d6424242222221216d5206111111111160025d64242465d644446d69f660000000000000000000000000000000000000000000000000
0000000000066f99494964242222221211116d52061e1e1e1e1e60025d62224265d642446d649ff6000000000000000000000000000000000000000000000000
00000000006f9994944462422221211111116d5206e1e1e1e1e160025d62222265d624426d624946d66600000000000000000000000000000000000000000000
0000000006994944444462222212111116666d5206eeeeeeeeee60025d61212165d622246d642496d6ff60000000000000000000000000000000000000000000
000000000694944242426212111116666ddddd52061ee1ee1ee160025d61111165d622226d624246d699f6600000000000000000000000000000000000000000
0000000006444424222261111e1e6dddd55555206eeeeeeeeeeee6025d61e1e165d621216d642426d6949ff66000000000000000000000000000000000000000
0000666d56424242226661e1e1ee6d52000000006eeeeeeeeeeee6025d6e1e1e65d611126d622246d649494ff600000000000000000000000000000000000000
00669f6d5624222666d56e1eeeee6d666d5000006eeeeeeeeeeee6025d6eeeee65d6e1e16d622426d62444949f66000000000000000000000000000000000000
06f9496d5622226d52006eeeeeee66eee6d520006eeeeeeeeeeee6025d6eeeee65d61e1e6d612226d642424449ff600000000000000000000000000000000000
0694946d5621216d52006eeeeeeeeeeeee6d52006eeeeeeeeeeee6025d6eeeee65d6eeee6d6e1126d62424244499f66000000000000000000000000000000000
0644446d56111e6d666d6eeeeeeeeeeeeee6d526eeeeee66eeeeee625d6eeeee65d6eeee6d61e1165d66224242449ff600000000000000000000000000000000
0624226d561eeee6eee66eeeeeeeeeeeeeee6d56eeeee6dd6eeeee625d6eeeee65d6eeee6d6e1e1625dd66242424499600000000000000000000000000000000
0622216d56eeeeeeeeee6eeeeeeeeeeeeeee6d56eeeee6dd6eeeee625d6eeeee65d6eeee6d6eeee60225dd662242424600000000000000000000000000000000
06221116d6eeeeeeeeee66eeeeeeeeeeeeee6d56eeeee6dd6eeeee625d6eeeee65d6eeee6d6eeee6000255d62121242600000000000000000000000000000000
0611e1e6d6eeeeeeeeee6d6ee666eeeeeeee6d56eeeee6556eeeee625d6eeeee65d6eeee6d6eeee6000025d61212662600000000000000000000000000000000
61eeeee6d6eeeeeeeeee6d566006eeeeeeee6d6eeeeee6556eeeeee65d6eeeee65d6eeee6d6eeee6000025d611116d6600000000000000000000000000000000
6eeeeee6d6eeeeeeeeee6d520006eeeeeeee6d6eeeeeeeeeeeeeeee65d6eeeee65d6eeee6d6eeee6000025d6e1e1600000000000000000000000000000000000
6eeeeee6d56ee66eeeee6d520006eeeeeeee6d6eeeeeeeeeeeeeeee65d6eeeee65d6eeee6d6eeee6000025d6eeee600000000000000000000000000000000000
6ee6eee6d526606eeeee6d520006eeeeeeee6d6eeeeeeeeeeeeeeee65d6eeeee65d6eeee6d6eeee6000025d6eeee600000000000000000000000000000000000
6ee6eee6d520006eeeee6d566666eeeeeeee6d6eeeeeeeeeeeeeeee65d6eeeee6666eeee6d6eeee6000025d6eeee600000000000000000000000000000000000
6ee6eee6d520006eeeee666eeeeeeeeeeeee6d6eeeeeeeeeeeeeeee65d6eeeeeeeeeeeee6d6eeee6666025d6eeee600000000000000000000000000000000000
6eeeeee6d566666eeeee6eeeeeeeeeeeeeee6d6eeeeeeeeeeeeeeee65d6eeeeeeeeeeeee6d6eeeeeeee66666eeee600000000000000000000000000000000000
6eeeeeee66eeeeeeeeee6eeeeeeeeeeeeeee6d6eeeeeeeeeeeeeeee65d6eeeeeeeeeeeee6d6eeeeeeeeeeee6eeee600000000000000000000000000000000000
6eeeeeee6eeeeeeeeeee6eeeeeeeeeeeeeee66eeeeeee6666eeeeeee6d6eeeeeeeeeeeee6d6eeeeeeeeeeee6eeee600000000000000000000000000000000000
6eeeeeee6eeeeeeeeee66eeeeeeeeeeeeee6d6eeeeee6dddd6eeeeee65d6eeeeeeeeeee65d6eeeeeeeeeeee6eeee600000000000000000000000000000000000
6eeeeeee6eeeeeeeeee6d6eeeeeeeeeeee6d56eeeeee6dddd6eeeeee625d6eeeeeeeeee65d6eeeeeeeeeeee6eeee600000000000000000000000000000000000
6ee66eee6eeeeeeeee6d526eeeeeeeeeee6d56eeeeee6d55d6eeeeee60256eeeeeeeeee65d6eeeeeeeeeeee6eeee600000000000000000000000000000000000
6ee66eee6eeeeeeeee6d526eeeeeeeeee6d526eeeeee6d55d6eeeeee6025d6eeeeeeee625d6eeeeeeeeeeee6eeee600000000000000000000000000000000000
666666666666666666d52006666666666d52066666666d55d666666660025d66666666025d666666666666666666600000000000000000000000000000000000
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

__map__
0000020304050607080000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001112131415161718191a1b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
202122232425262728292a2b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
303132333435363738393a3b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
404142434445464748494a4b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
