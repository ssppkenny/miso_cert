#!/usr/bin/env python3
import re
import sys

def extract_chapter_questions(ocp_file, chapter_num):
    """Extract all questions from a specific chapter in ocp_tests.txt"""
    with open(ocp_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    questions = {}
    in_chapter = False
    current_q = None
    current_text = []
    
    # Patterns to match chapters
    chapter_pattern = re.compile(r'Chapter\s+' + str(chapter_num) + r'\s')
    next_chapter_pattern = re.compile(r'Chapter\s+' + str(chapter_num + 1) + r'\s')
    
    for i, line in enumerate(lines):
        # Check if we're entering our chapter
        if chapter_pattern.search(line) and not in_chapter:
            in_chapter = True
            continue
        
        # Check if we hit the next chapter
        if in_chapter and next_chapter_pattern.search(line):
            # Save last question
            if current_q is not None and current_text:
                questions[current_q] = '\n'.join(current_text)
            break
        
        if in_chapter:
            # Look for question starts (number followed by period at start, with optional spaces)
            q_match = re.match(r'^\s*(\d+)\.\s+', line)
            if q_match:
                # Save previous question
                if current_q is not None and current_text:
                    questions[current_q] = '\n'.join(current_text)
                
                current_q = int(q_match.group(1))
                current_text = [line.rstrip()]
            elif current_q is not None:
                current_text.append(line.rstrip())
    
    # Don't forget last question
    if current_q is not None and current_text:
        questions[current_q] = '\n'.join(current_text)
    
    return questions

def count_answers(question_text):
    """Count answer options (A., B., C., etc.) in a question"""
    lines = question_text.split('\n')
    count = 0
    for line in lines:
        # Match lines that start with optional spaces, then A., B., C., etc.
        if re.match(r'^\s*[A-Z]\.\s+', line):
            count += 1
    return count

def extract_our_questions(questions_file):
    """Extract questions from our questions.txt file"""
    with open(questions_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    questions = {}
    current_q = None
    current_text = []
    
    for line in lines:
        q_match = re.match(r'^(\d+)\.\s+', line)
        if q_match:
            # Save previous question
            if current_q is not None and current_text:
                questions[current_q] = '\n'.join(current_text)
            
            current_q = int(q_match.group(1))
            current_text = [line.rstrip()]
        elif current_q is not None:
            current_text.append(line.rstrip())
    
    # Don't forget last question
    if current_q is not None and current_text:
        questions[current_q] = '\n'.join(current_text)
    
    return questions

# Main execution
ocp_file = '/home/sergey/code/haskell/cert/ocp_tests.txt'

problems_found = []

for chapter in range(6, 15):
    questions_file = f'/home/sergey/code/haskell/cert/resources/chapter{chapter}/questions.txt'
    
    try:
        ocp_questions = extract_chapter_questions(ocp_file, chapter)
        our_questions = extract_our_questions(questions_file)
        
        print(f"\n{'='*80}")
        print(f"Chapter {chapter}")
        print(f"{'='*80}")
        
        chapter_problems = []
        
        for q_num in sorted(set(ocp_questions.keys()) | set(our_questions.keys())):
            ocp_text = ocp_questions.get(q_num, '')
            our_text = our_questions.get(q_num, '')
            
            ocp_count = count_answers(ocp_text)
            our_count = count_answers(our_text)
            
            if ocp_count != our_count and ocp_count > 0:
                problem = {
                    'chapter': chapter,
                    'question': q_num,
                    'ocp_count': ocp_count,
                    'our_count': our_count,
                    'ocp_text': ocp_text,
                    'our_text': our_text
                }
                chapter_problems.append(problem)
                print(f"Q{q_num}: OCP has {ocp_count} answers, we have {our_count}")
        
        if not chapter_problems:
            print("✓ All questions match!")
        else:
            problems_found.extend(chapter_problems)
            
    except FileNotFoundError:
        print(f"File not found: {questions_file}")

print(f"\n{'='*80}")
print(f"Total problems found: {len(problems_found)}")
print(f"{'='*80}")

# Show detailed info for first few problems
if problems_found:
    print("\nShowing details for first 3 problems:")
    for i, problem in enumerate(problems_found[:3]):
        print(f"\n{'='*80}")
        print(f"Chapter {problem['chapter']}, Question {problem['question']}")
        print(f"OCP: {problem['ocp_count']} answers, Ours: {problem['our_count']} answers")
        print(f"{'='*80}")
        print("\nOCP version:")
        print(problem['ocp_text'][:500] + "..." if len(problem['ocp_text']) > 500 else problem['ocp_text'])
        print("\nOur version:")
        print(problem['our_text'][:500] + "..." if len(problem['our_text']) > 500 else problem['our_text'])

