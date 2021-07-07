local file = io.open("test_benchmark.lang", "w")
io.output(file)

local func_count = 10000
local stmt_count = 100

io.write("def extern puts(arg: *u8);")

for i = 1,func_count do
	io.write(string.format("def func%d() {\n", i))
	for j = 1,stmt_count do
		io.write("\tputs(\"hello, world!\");\n")
	end
	io.write("}\n\n")
end

io.write("def export main() {\n")
for i = 1,func_count do
	io.write(string.format("\tfunc%d();\n", i))
end
io.write("}\n")

io.close(file)
