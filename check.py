"""
    This module is used for testing in CS 116.

    The useful functions in this module are:
    * check.within, for testing functions that produce floats
    * check.expect, for testing all other functions
    * check.set_screen, for testing screen output (print statements)
    * check.set_input, for testing keyboard input (raw_input)
    * check.set_files, for testing file output

    For details on using these functions, please read
    the Python Style guide from the CS 116 website,
    www.student.cs.uwaterloo.ca/~cs116/styleGuide
    
    The majority of this file was written by staff of the University of
    Waterloo.  All differences from the version distributed by the CS
    departement were written by Joel Williamson. Anyone may freely
    copy, modify and redistribute these changes.
"""

import sys, os
backup_stdin = sys.stdin
backup_stdout = sys.stdout

class InputError(Exception):
    """
    List given to set_input was too short.
    """
    def __init__(self):
        self.val = "no elements remaining in input_list"
    def __str__(self):
        return self.val

class redirect_input:
    """
    Keyboard input is redirected from this class
    whenever set_input is called.
    """
    def __init__(self, inp):
        self.lst = inp
    def readline(self):
        if self.lst:
            return self.lst.pop(0)
        else:
            raise InputError()

class redirect_output:
    """
    Screen output is redirected to this class
    whenever set_screen is called.
    """
    def __init__(self):
        self.screen = ""
    def __str__(self):
        return self.screen
    def __nonzero__(self):
        return bool(self.screen)
    def write(self, string):
        self.screen += string
    def reset(self):
        self.screen = ""

expected_screen = ""
actual_screen = redirect_output()
test_output = redirect_output()
input_list = []
file_list = []
dir_list = []

def set_screen(string):
    """
    Consumes a description of the expected screen output
    for the next call to check.expect or check.within.
    """
    global expected_screen
    expected_screen = string
    sys.stdout = actual_screen

def set_input(lst):
    """
    Consumes a list containing the keyboard input for
    the next call to check.expect or check.within.
    """
    global input_list
    if type(lst) != list:
        raise TypeError("set_input must consume a list")
    for i in lst:
        if type(i) != str:
            raise TypeError("all elements of input_list must be strings")
    input_list = map(lambda s: s+"\n", lst)
    sys.stdin = redirect_input(input_list)
    sys.stdout = actual_screen

def set_file(resulting_file, expected_file):
    """
    Consumes two strings: resulting_file (the name
    of a file that will be produced by the function)
    and expected_file (the name of a file to whose
    contents will match resulting_file if the function
    works correctly). Checks that the files contain the
    same text, ignoring whitespace, on the next call
    to check.expect or check.within.
    """
    global file_list, dir_list
    dir_list = os.listdir(os.getcwd())
    file_list.append((resulting_file, expected_file, False))

def set_file_exact(resulting_file, expected_file):
    """
    Consumes two strings: resulting_file (the name
    of a file that will be produced by the function)
    and expected_file (the name of a file to whose
    contents will match resulting_file if the function
    works correctly). Checks that the files contain the
    same text, including whitespace, on the next call
    to check.expect or check.within.
    """
    global file_list, dir_list
    dir_list = os.listdir(os.getcwd())
    file_list.append((resulting_file, expected_file, True))

def expect(label, function_call, expected_value):
    """
    Testing function for equality. Will also print the
    description given to set_screen, use the keyboard
    input given to set_input, and compare files given
    to set_files.
    """
    run_test(label, function_call, expected_value, None)

def within(label, function_call, expected_value, acceptable_tolerance):
    """
    Testing function for floating point numbers. Will also
    print the description given to set_screen, use the
    keyboard input given to set_input, and compare files
    given to set_files.
    """
    run_test(label, function_call, expected_value, acceptable_tolerance)

def run_test(label, result, expected, tolerance):
    """
    Performs the tests given to check.within and check.expect.
    Do not use run_test in your code for CS 116.
    """
    global actual_screen, expected_screen, input_list, file_list, dir_list
    sys.stdout = test_output
    
    if input_list:
        print "%s: FAILED; not all strings in input_list were used\n" % label
    elif tolerance and abs(result - expected) > tolerance:
        print "%s: FAILED; expected %s, saw %s\n" \
              % (label, expected, result)
    elif not(tolerance) and result != expected:
        print "%s: FAILED; expected %s, saw %s\n" \
              % (label, expected, result)
    else :
        print "%s: The test passed" % (label)
    
    if file_list:
        new_files = []
        for tup in file_list:
            new_label = "%s %r" % (label, tup[0:2])
            compare_files(new_label, new_files, tup[0], tup[1], tup[2])
        extra_files = list(set(os.listdir(os.getcwd())) ^ set(dir_list + new_files) )
        if extra_files:
            print "%s: The following additional files were created: %s"\
                  % (label, ", ".join(extra_files))
            
    ## differences: redirect_ouput redirect_ouput -> (tuple bool (listof str))
    ## Purpose: differences consumes two redirect_ouputs and produces a tuple
    ## containing a bool (true if the outputs differ, false otherwise) and a list
    ## of strings describing the differences (empty if they are equal)
    def differences (expected,actual) :
        different = False
        differences = []
        if ( len(expected.__str__()) != len(actual.__str__())) :
            differences.append("Different length")
            different = True
        else :
            for i in range(len(expected.__str__())) :
                if expected.__str__()[i]!=actual.__str__()[i] :
                    differences.append("(" + expected.__str__()[i] + "," + actual.__str__()[i] + ")")
                    different = True
        return (different,differences)

    if expected_screen :
        print "%s (expected screen output):" % label
        print expected_screen
        print ""
        print "%s (actual screen output):" % label
        print actual_screen
        delta = differences(expected_screen,actual_screen)
        if (delta[0]) :
            print "The following are different: "
            for diff in delta[1] :
                print diff
        else :
            print "The actual output matches the expected output."
            

    input_list, file_list, dir_list = [], [], []
    expected_screen = ""
    actual_screen.reset()
    
    sys.stdin = backup_stdin
    sys.stdout = backup_stdout
    if test_output:
        print str(test_output).strip()
        print "\n-----\n"
    test_output.reset()


def compare_files(label, new_files, fname1, fname2, exact):
    """
    Performs file comparisons for check.within and check.expect.
    Do not use compare_files in your code for CS 116.
    """
    try:
        f = file(fname1, 'r')
        lines1 = map(lambda x: x.strip(), f.readlines())
        f.close()
        new_files.append(fname1)
    except IOError:
        print "%s: File %s does not exist\n"\
              % (label, fname1)
        return None
    try:
        f = file(fname2, 'r')
        lines2 = map(lambda x: x.strip(), f.readlines())
        f.close()
        new_files.append(fname2)
    except IOError:
        print "%s: File %s does not exist\n"\
              % (label, fname2)
        return None
    
    if lines1 == [] and lines2 == []:
        return None
    elif lines1 == []:
        print "%s: %s is empty but %s is not."\
              % (label, fname1, fname2)
        print "%s (line 1): %s\n" % (fname2, lines2[0])
        return None
    elif lines2 == []:
        print "%s: %s is empty but %s is not."\
              % (label, fname2, fname1)
        print "%s (line 1): %s\n" % (fname1, lines1[0])
        return None
    
    while lines1[-1].isspace() or lines1[-1] == "":
        lines1.pop()
    while lines2[-1].isspace() or lines2[-1] == "":
        lines2.pop()
    
    if len(lines1) != len(lines2):
        print "%s: %s and %s do not contain the same number of lines."\
              % (label, fname1, fname2)
    
    n = min(len(lines1), len(lines2))
    bad_lines = []
    
    for i in range(n):
        if exact:
            line1 = lines1[i].rstrip()
            line2 = lines2[i].rstrip()
        else:
            line1 = "".join(lines1[i].split())
            line2 = "".join(lines2[i].split())
        if line1 != line2:
            bad_lines.append(i+1)
    
    if bad_lines:
        first_line = bad_lines[0]
        bad_lines = ", ".join(map(lambda x: str(x), bad_lines))
        print "%s: The following lines in %s and %s do not match: %s"\
              % (label, fname1, fname2, bad_lines)
        
        extra_spaces = " " * abs(len(fname1) - len(fname2))
        if len(fname1) < len(fname2):
            fname1 += extra_spaces
        else:
            fname2 += extra_spaces
        
        print "%s (line %s): %s" % (fname1, first_line, lines1[first_line-1])
        print "%s (line %s): %s" % (fname2, first_line, lines2[first_line-1])
    
    if len(lines1) != len(lines2) or bad_lines:
        print ""
