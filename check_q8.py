#!/usr/bin/env python3
import re

# Read chapter 7 question 8 from ocp_tests.txt
with open('/home/sergey/code/haskell/cert/ocp_tests.txt', 'r', encoding='utf-8') as f:
    content = f.read()

# Find question 8 in chapter 7
# Looking for the specific question about "Which of the following statements are true"
start_idx = content.find("8.   Which of the following statements are true? (Choose two.)")
if start_idx == -1:
    print("Question 8 not found!")
    exit(1)

# Find the end (next question starts with "9.")
end_idx = content.find("\n9.   ", start_idx)

question_8_text = content[start_idx:end_idx]
print("Chapter 7, Question 8 from ocp_tests.txt:")
print("=" * 80)
print(question_8_text)
print("=" * 80)

# Now let's check what's in our questions.txt
with open('/home/sergey/code/haskell/cert/resources/chapter7/questions.txt', 'r', encoding='utf-8') as f:
    our_content = f.read()

# Find question 8
our_start = our_content.find("8.   Which of the following statements are true?")
if our_start == -1:
    print("\nQuestion 8 not found in our file!")
    exit(1)

our_end = our_content.find("\n9.", our_start)
our_question_8 = our_content[our_start:our_end]

print("\nChapter 7, Question 8 from our questions.txt:")
print("=" * 80)
print(our_question_8)
print("=" * 80)

