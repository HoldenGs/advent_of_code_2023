


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


#print(get_sum_of_first_last('input.txt'))


def get_sum_of_first_last_word_digits_included(input_file):
    """Returns the sum of the data"""
    nums = []
    word_to_num = {'one': 1, 'two': 2, 'three': 3, 'four': 4, 'five': 5,
                   'six': 6, 'seven': 7, 'eight': 8, 'nine': 9}
    with open(input_file) as f:
        data = f.readlines()
        for i in data:
            # get first digit in i string
            # check for the words one, two, three, four, five, six, seven, eight, nine
            num = ''
            first_idx = len(i)
            for idx, j in enumerate(i):
                if j.isdigit():
                    first_idx = idx
                    num += j
                    break
            filter_fun = lambda x: x[0] >= 0
            min_word_idx = min(filter(filter_fun, [(i.find('one'), 3), (i.find('two'), 3), (i.find('three'), 5), (i.find('four'), 4), (i.find('five'), 4), (i.find('six'), 3), (i.find('seven'), 5), (i.find('eight'), 5), (i.find('nine'), 4)]), default=(len(i), 0))
            if min_word_idx[0] < first_idx:
                # if there is a word before the first digit, get the word and convert to digit
                num = str(word_to_num[i[min_word_idx[0]:min_word_idx[0]+min_word_idx[1]]])
            # get last digit in i string
            last_idx = -1
            for idx, j in enumerate(reversed(i)):
                if j.isdigit():
                    last_idx = len(i) - idx - 1
                    num += j
                    break
            max_word_idx = max([(i.rfind('one'), 3), (i.rfind('two'), 3), (i.rfind('three'), 5), (i.rfind('four'), 4), (i.rfind('five'), 4), (i.rfind('six'), 3), (i.rfind('seven'), 5), (i.rfind('eight'), 5), (i.rfind('nine'), 4)])
            if max_word_idx[0] > last_idx:
                # if there is a word after the last digit, get the word and convert to digit
                if last_idx != -1:
                    num = num[:-1]
                num += str(word_to_num[i[max_word_idx[0]:max_word_idx[0]+max_word_idx[1]]])

            nums.append(int(num))
                
    return sum(nums)

print(get_sum_of_first_last_word_digits_included('input.txt'))
