#!/usr/bin/env python3
"""
Fix questions that are missing answer options in chapters 6-14.
This script:
1. Fixes questions with leading spaces (Ch6 Q68, Ch7 Q28, Ch10 Q14)
2. Adds missing answer options from ocp_tests.txt for questions with 0 answers
"""

import re
import os

# Chapter starting lines in ocp_tests.txt (line where Q1 of the chapter is)
CHAPTER_STARTS = {
    6:  9583 - 1,   # 0-indexed
    7:  11977 - 1,
    8:  13162 - 1,
    9:  14145 - 1,
    10: 14970 - 1,
    11: 15545 - 1,
    12: 16054 - 1,
    13: 17184 - 1,
    14: 18261 - 1,
}

CHAPTER_ENDS = {
    6:  11977 - 1,
    7:  13162 - 1,
    8:  14145 - 1,
    9:  14970 - 1,
    10: 15545 - 1,
    11: 16054 - 1,
    12: 17184 - 1,
    13: 18261 - 1,
    14: 26746,   # end of file
}


def parse_ocp_chapter(ocp_lines, chapter_num):
    """Parse questions from a chapter in ocp_tests.txt"""
    start = CHAPTER_STARTS[chapter_num]
    end = CHAPTER_ENDS[chapter_num]
    
    chapter_lines = ocp_lines[start:end]
    
    questions = {}
    current_q = None
    current_lines = []
    
    for line in chapter_lines:
        q_match = re.match(r'^\s*(\d+)\.\s+', line)
        if q_match:
            q_num = int(q_match.group(1))
            # Only add if this is a reasonable question number (avoid page headers with numbers)
            if current_q is not None:
                questions[current_q] = '\n'.join(current_lines)
            current_q = q_num
            current_lines = [line]
        elif current_q is not None:
            current_lines.append(line)
    
    if current_q is not None:
        questions[current_q] = '\n'.join(current_lines)
    
    return questions


def get_answers_from_text(text):
    """Extract answer letters from text"""
    answers = []
    for line in text.split('\n'):
        m = re.match(r'^\s*([A-Z])\.\s+', line)
        if m:
            answers.append(m.group(1))
    return answers


def extract_answer_lines(text):
    """Extract the answer option lines from ocp question text"""
    lines = text.split('\n')
    answer_lines = []
    in_answers = False
    
    for line in lines:
        m = re.match(r'^\s*([A-Z])\.\s+(.+)', line)
        if m:
            in_answers = True
            letter = m.group(1)
            content = m.group(2).strip()
            answer_lines.append(f'{letter}.  {content}')
        elif in_answers and line.strip() and not re.match(r'^\d+\.\s+', line.strip()):
            # Continuation of previous answer (ignore page headers)
            if not re.match(r'^\s*\d{3}\s+Chapter', line) and not re.match(r'^Chapter\s+\d+', line.strip()):
                if answer_lines:
                    answer_lines[-1] += ' ' + line.strip()
        elif in_answers and re.match(r'^\d+\.\s+', line.strip()):
            break  # Next question started
    
    return answer_lines


def fix_leading_spaces(content):
    """Fix questions that have leading spaces before question number"""
    lines = content.split('\n')
    fixed = []
    for line in lines:
        stripped = line.lstrip()
        # Check if this line is a question with leading spaces (2+ spaces before number)
        if len(line) - len(stripped) >= 2 and re.match(r'\d+\.\s+', stripped):
            fixed.append(stripped)
        else:
            fixed.append(line)
    return '\n'.join(fixed)


def add_missing_answers(our_content, ocp_questions, chapter_num):
    """Add missing answer options to questions that have none"""
    
    # Parse our questions
    our_lines = our_content.split('\n')
    result_lines = []
    
    i = 0
    while i < len(our_lines):
        line = our_lines[i]
        q_match = re.match(r'^(\d+)\.\s+', line)
        
        if q_match:
            q_num = int(q_match.group(1))
            
            # Collect this question's existing content
            q_start = i
            q_content_lines = [line]
            i += 1
            
            while i < len(our_lines) and not re.match(r'^\d+\.\s+', our_lines[i]):
                q_content_lines.append(our_lines[i])
                i += 1
            
            q_text = '\n'.join(q_content_lines)
            our_answers = get_answers_from_text(q_text)
            
            if not our_answers and q_num in ocp_questions:
                # Get answers from ocp_tests.txt
                ocp_text = ocp_questions[q_num]
                ocp_answer_lines = extract_answer_lines(ocp_text)
                
                if ocp_answer_lines:
                    # Add the content we have, then the answers
                    result_lines.extend(q_content_lines)
                    result_lines.extend(ocp_answer_lines)
                    print(f"  Chapter {chapter_num} Q{q_num}: Added {len(ocp_answer_lines)} answers")
                else:
                    result_lines.extend(q_content_lines)
                    print(f"  Chapter {chapter_num} Q{q_num}: No answers found in OCP either!")
            else:
                result_lines.extend(q_content_lines)
        else:
            result_lines.append(line)
            i += 1
    
    return '\n'.join(result_lines)


def main():
    ocp_file = '/home/sergey/code/haskell/cert/ocp_tests.txt'
    base_dir = '/home/sergey/code/haskell/cert/resources'
    
    print("Loading ocp_tests.txt...")
    with open(ocp_file, 'r', encoding='utf-8') as f:
        ocp_lines = f.readlines()
    # Strip newlines but keep them accessible
    ocp_lines_stripped = [l.rstrip('\n') for l in ocp_lines]
    
    print(f"Total lines in ocp_tests.txt: {len(ocp_lines_stripped)}")
    
    for chapter in range(6, 15):
        questions_file = f'{base_dir}/chapter{chapter}/questions.txt'
        print(f"\nProcessing Chapter {chapter}...")
        
        # Read our questions file
        with open(questions_file, 'r', encoding='utf-8') as f:
            our_content = f.read()
        
        # Step 1: Fix leading spaces
        fixed_content = fix_leading_spaces(our_content)
        if fixed_content != our_content:
            print(f"  Fixed leading spaces")
        
        # Step 2: Parse OCP questions for this chapter
        ocp_questions = parse_ocp_chapter(ocp_lines_stripped, chapter)
        
        # Step 3: Add missing answers
        final_content = add_missing_answers(fixed_content, ocp_questions, chapter)
        
        # Write back
        if final_content != our_content:
            # Make backup
            backup_file = questions_file + '.bak'
            with open(backup_file, 'w', encoding='utf-8') as f:
                f.write(our_content)
            
            with open(questions_file, 'w', encoding='utf-8') as f:
                f.write(final_content)
            print(f"  Saved changes (backup: {backup_file})")
        else:
            print(f"  No changes needed")
    
    print("\nDone! Verifying results...")
    
    # Verify
    for chapter in range(6, 15):
        questions_file = f'{base_dir}/chapter{chapter}/questions.txt'
        with open(questions_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        lines = content.split('\n')
        questions = {}
        current_q = None
        current_lines = []
        for line in lines:
            q_match = re.match(r'^(\d+)\.\s+', line)
            if q_match:
                if current_q is not None:
                    questions[current_q] = '\n'.join(current_lines)
                current_q = int(q_match.group(1))
                current_lines = [line]
            elif current_q is not None:
                current_lines.append(line)
        if current_q is not None:
            questions[current_q] = '\n'.join(current_lines)
        
        no_ans = [q for q, t in sorted(questions.items()) if not get_answers_from_text(t)]
        if no_ans:
            print(f"Chapter {chapter}: Still {len(no_ans)} questions with no answers: {no_ans}")
        else:
            print(f"Chapter {chapter}: ✓ All questions have answers")


if __name__ == '__main__':
    main()

