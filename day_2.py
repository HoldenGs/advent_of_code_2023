
def get_max_cubes(cubes):
    """get the maximum number of cubes for each color

    Args:
        cubes (str): string of the form "q red, r blue, s green; m red, n blue, o green"
    """
    pulls = cubes.split(';')
    max_blue = 0
    max_red = 0
    max_green = 0
    
    for pull in pulls:
        for cube in pull.split(','):
            if 'red' in cube:
                max_red = max(max_red, int(cube.split(' ')[1]))
            elif 'blue' in cube:
                max_blue = max(max_blue, int(cube.split(' ')[1]))
            elif 'green' in cube:
                max_green = max(max_green, int(cube.split(' ')[1]))
                
    return max_red, max_blue, max_green
    

def get_possible_games(input_file, max_cubes):
    """Returns the number of possible games"""
    with open(input_file) as f:
        data = f.readlines()
        possible_games = []
        for line in data:
            game = int(line.split(':')[0].split(' ')[1])
            mred, mblue, mgreen = get_max_cubes(line.split(':')[1])
            if mred <= max_cubes['red'] and mblue <= max_cubes['blue'] and mgreen <= max_cubes['green']:
                possible_games.append(game)
    
    return possible_games
            
        
print(sum(get_possible_games('input_day_2.txt', {"red": 12, "blue": 14, "green": 13})))
