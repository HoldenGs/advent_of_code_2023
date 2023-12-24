
import re

test_input = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""

def split_line_regex(line):
    pattern = r'(\d+)|(\D)'
    nums = []
    symbols = []

    for match in re.finditer(pattern, line):
        if match.group(1):
            # It's a sequence of digits
            nums.append((match.start(), match.group(1)))
        elif match.group(2) != '.':
            # It's a non-digit character
            symbols.append((match.start()))

    return nums, symbols

def find_adjacent(num, nums, symbols):
    # if num[2] == '577':
    #     print(num)
    for x in range(num[0] - 1, num[0]+len(num[2])+1):
        for y in range(num[1] - 1, num[1]+2):
            if (x, y) in symbols:
                # if num[2] == '577' and y == 1:
                #     print('found symbol')
                return int(num[2])
    return 0

def part_sum(input):
    """gets all part numbers adjacent to a symbol and adds them together"""
    acc = 0
    part_numbers = []
    nums = []
    symbols = []
    for y, line in enumerate(input.split('\n')):
        nums_line, symbols_line = split_line_regex(line)
        nums += list(map(lambda x: (x[0], y, x[1]), nums_line))
        symbols += list(map(lambda x: (x, y), symbols_line))
    
    # print(symbols[3])
    nums = nums
    symbols = set(symbols)
    
    for num in nums:
        new_num = find_adjacent(num, nums, symbols)
        acc += new_num
        if new_num != 0:
            part_numbers.append(new_num)

    # print(part_numbers)
    #print(symbols[0])
    with open('part_numbers.txt', 'w') as f:
        for num in part_numbers:
            f.write(str(num) + '\n')
    return acc
   
        
print(part_sum(test_input))

    
with open('input_day_3.txt') as f:
    data = f.read()
    print(part_sum(data))