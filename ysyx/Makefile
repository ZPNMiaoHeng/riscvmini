Tilefile = ../generated-src/Tile.v
TOP = ./ysyx_mini_top.v

# 将 Tilefile 与 TOP文件内容合并
ysyx:
	cat $(Tilefile) $(TOP) > ./build/ysyx_mini.v
#	cp ./build/ysyx_mini.v /home/zpn/Documents/Pro/Projects/mini/iverilog-soc/cpu
	cp ./build/ysyx_mini.v /home/zpn/Documents/Pro/Projects/ysyxSOC/ysyx/soc/
clean:
	rm -rf ./build/*