


def get_sum_of_first_last(input_file):
    """Returns the sum of the data"""
    nums = []
    with open(input_file) as f:
        data = f.readlines()
        for i in data:
            # get first digit in i string
            num = ''
            for j in i:
                if j.isdigit():
                    num += j
                    break
            # get last digit in i string
            for j in reversed(i):
                if j.isdigit():
                    num += j
                    break
            nums.append(int(num))
                
    return sum(nums)


print(get_sum_of_first_last('input.txt'))