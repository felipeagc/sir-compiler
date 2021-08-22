-- local file = io.open("test_benchmark.lang", "w")
-- io.output(file)

-- local depth = 200
-- local func_count = 200

-- io.write("fn extern vararg printf(_: *u8);\n\n")

-- for i = 1,func_count do
-- 	for j = 1,depth do
-- 		if j == 1 then
-- 			io.write(string.format("fn add_long_n%d_h%d(x: u64): u64 { return x + u64(15440); }\n", i-1, j-1))
-- 		else
-- 			io.write(string.format("fn add_long_n%d_h%d(x: u64): u64 { return x + add_long_n%d_h%d(x) + u64(15440); }\n", i-1, j-1, i-1, j-2))
-- 		end
-- 	end
-- 	io.write("\n")
-- end

-- io.write("fn export main(): u64 {\n")
-- io.write("\tlong := u64(0);\n")
-- for i = 1,func_count do
-- 	io.write(string.format("\tlong = long + add_long_n%d_h%d(%d);\n", i-1, depth-1, i-1))
-- end
-- io.write("\treturn long;\n")
-- io.write("}\n")

-- io.close(file)



-- local file = io.open("test_benchmark.lang", "w")
-- io.output(file)

-- local func_count = 1000
-- local stmt_count = 1000

-- io.write("fn extern puts(arg: *u8);\n\n")

-- for i = 1,func_count do
-- 	io.write(string.format("fn func%d() {\n", i))
-- 	for j = 1,stmt_count do
-- 		io.write("\tputs(\"hello, world!\");\n")
-- 	end
-- 	io.write("}\n\n")
-- end

-- io.write("fn export main() {\n")
-- for i = 1,func_count do
-- 	io.write(string.format("\tfunc%d();\n", i))
-- end
-- io.write("}\n")

-- io.close(file)



-- local file = io.open("test_benchmark.lang", "w")
-- io.output(file)

-- local func_count = 1000
-- local stmt_count = 500

-- io.write("fn extern vararg printf(arg: *u8);\n\n")

-- for i = 1,func_count do
-- 	io.write(string.format("fn func%d() {\n", i))
-- 	for j = 1,stmt_count do
-- 		io.write(string.format("\tvar a%d: [1 + 1]i32 = undefined;\n", j))
-- 		io.write("\tprintf(\"hello, world!\");\n")
-- 	end
-- 	io.write("}\n\n")
-- end

-- io.write("fn export main() {\n")
-- for i = 1,func_count do
-- 	io.write(string.format("\tfunc%d();\n", i))
-- end
-- io.write("}\n")

-- io.close(file)

local file = io.open("test_benchmark.lang", "w")
io.output(file)

local func_count = 10000

io.write("fn extern vararg printf(arg: *u8);\n\n")

for i = 1,func_count do
	io.write(string.format("fn func%d(x: u64) {\n", i))
	io.write([[
	var n = x;
	var i = u32(0);
	while (i < 8) {
		printf("%02x", (n >> ((7 - i) * 8)) & 0xff);
		i = i + 1;
	}
	printf("\n");]])
	io.write("\n}\n\n")
end

io.write("fn export main() {\n")
for i = 1,func_count do
	io.write(string.format("\tfunc%d(%d);\n", i, i))
end
io.write("}\n")

io.close(file)
