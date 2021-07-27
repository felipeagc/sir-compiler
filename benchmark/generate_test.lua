local file = io.open("test_benchmark.lang", "w")
io.output(file)

-- local func_count = 10000
-- local stmt_count = 100

local depth = 100
local func_count = 100

io.write("def extern vararg printf(_: *u8);\n\n")

for i = 1,func_count do
	for j = 1,depth do
		if j == 1 then
			io.write(string.format("def add_long_n%d_h%d(x: u64): u64 { return x + u64(15440); }\n", i-1, j-1))
		else
			io.write(string.format("def add_long_n%d_h%d(x: u64): u64 { return x + add_long_n%d_h%d(x) + u64(15440); }\n", i-1, j-1, i-1, j-2))
		end
	end
	io.write("\n")
end

io.write("def export main(): u64 {\n")
io.write("\tlong := u64(0);\n")
for i = 1,func_count do
	io.write(string.format("\tlong = long + add_long_n%d_h%d(%d);\n", i-1, depth-1, i-1))
end
io.write("\treturn long;\n")
io.write("}\n")

io.close(file)
