#!/usr/bin/env python3
import re
import os

def parse_questions_from_file(filepath):
    """Parse questions and count their answers from a questions.txt file"""
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()
    
    questions = {}
    lines = content.split('\n')
    
    current_question = None
    answer_count = 0
    
    for line in lines:
        # Match question number (e.g., "1.   What is...")
        q_match = re.match(r'^(\d+)\.\s+', line)
        if q_match:
            if current_question is not None:
                questions[current_question] = answer_count
            current_question = int(q_match.group(1))
            answer_count = 0
        # Match answer options (A., B., C., etc.)
        elif re.match(r'^[A-Z]\.\s+', line):
            answer_count += 1
    
    # Don't forget the last question
    if current_question is not None:
        questions[current_question] = answer_count
    
    return questions

def parse_questions_from_ocp(filepath, chapter_num):
    """Parse questions from ocp_tests.txt for a specific chapter"""
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    questions = {}
    in_chapter = False
    current_question = None
    answer_count = 0
    
    for i, line in enumerate(lines):
        # Check if we're entering the chapter
        if re.search(f'Chapter {chapter_num}\\s+', line):
            in_chapter = True
            continue
        
        # Check if we're leaving the chapter (next chapter starts)
        if in_chapter and re.search(f'Chapter {chapter_num + 1}\\s+', line):
            break
        
        if in_chapter:
            # Match question number
            q_match = re.match(r'^(\d+)\.\s+', line)
            if q_match:
                if current_question is not None:
                    questions[current_question] = answer_count
                current_question = int(q_match.group(1))
                answer_count = 0
            # Match answer options
            elif re.match(r'^\s*[A-Z]\.\s+', line):
                answer_count += 1
    
    # Don't forget the last question
    if current_question is not None:
        questions[current_question] = answer_count
    
    return questions

# Main execution
ocp_file = '/home/sergey/code/haskell/cert/ocp_tests.txt'
base_dir = '/home/sergey/code/haskell/cert/resources'

for chapter in range(6, 15):  # Chapters 6-14
    chapter_dir = f'{base_dir}/chapter{chapter}'
    questions_file = f'{chapter_dir}/questions.txt'
    
    if not os.path.exists(questions_file):
        print(f"Chapter {chapter}: questions.txt not found")
        continue
    
    our_questions = parse_questions_from_file(questions_file)
    ocp_questions = parse_questions_from_ocp(ocp_file, chapter)
    
    print(f"\n=== Chapter {chapter} ===")
    
    # Find discrepancies
    discrepancies = []
    for q_num in sorted(set(our_questions.keys()) | set(ocp_questions.keys())):
        our_count = our_questions.get(q_num, 0)
        ocp_count = ocp_questions.get(q_num, 0)
        
        if our_count != ocp_count:
            discrepancies.append(f"  Q{q_num}: Our file has {our_count} answers, OCP has {ocp_count}")
    
    if discrepancies:
        print(f"Found {len(discrepancies)} questions with different answer counts:")
        for disc in discrepancies:
            print(disc)
    else:
        print("All questions have matching answer counts!")

