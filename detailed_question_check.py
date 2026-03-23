#!/usr/bin/env python3
"""
This script finds questions that are completely missing or have incomplete answers
by comparing our questions.txt files with ocp_tests.txt.
"""

import re

def extract_chapter_questions_detailed(ocp_file, chapter_num):
    """Extract all questions from a specific chapter in ocp_tests.txt with full details"""
    with open(ocp_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Find the chapter start - look for the actual questions section
    chapter_markers = [
        f'Chapter {chapter_num}  ',
        f'Chapter{chapter_num}',
        f'{chapter_num}  Chapter {chapter_num}'
    ]
    
    chapter_start = -1
    for marker in chapter_markers:
        idx = content.find(marker)
        if idx != -1:
            # Find where question 1 starts after this marker
            q1_pattern = re.compile(r'\n\s*1\.\s+', re.MULTILINE)
            match = q1_pattern.search(content, idx)
            if match:
                chapter_start = match.start()
                break
    
    if chapter_start == -1:
        print(f"Could not find start of Chapter {chapter_num}")
        return {}
    
    # Find the next chapter
    next_chapter_pattern = re.compile(f'Chapter {chapter_num + 1}\\s+', re.MULTILINE)
    next_match = next_chapter_pattern.search(content, chapter_start + 100)
    chapter_end = next_match.start() if next_match else len(content)
    
    chapter_content = content[chapter_start:chapter_end]
    
    # Extract questions
    questions = {}
    lines = chapter_content.split('\n')
    
    current_q = None
    current_lines = []
    
    for line in lines:
        # Match question number at start of line
        q_match = re.match(r'^\s*(\d+)\.\s+', line)
        if q_match:
            if current_q is not None:
                questions[current_q] = '\n'.join(current_lines)
            current_q = int(q_match.group(1))
            current_lines = [line]
        elif current_q is not None:
            current_lines.append(line)
    
    # Save last question
    if current_q is not None:
        questions[current_q] = '\n'.join(current_lines)
    
    return questions

def count_answers_in_text(text):
    """Count answer options in text"""
    lines = text.split('\n')
    answers = []
    for line in lines:
        match = re.match(r'^\s*([A-Z])\.\s+', line)
        if match:
            answers.append(match.group(1))
    return answers

# Main execution
ocp_file = '/home/sergey/code/haskell/cert/ocp_tests.txt'

print("Analyzing chapters 6-14 for missing or incomplete questions...\n")

all_issues = []

for chapter in range(6, 15):
    questions_file = f'/home/sergey/code/haskell/cert/resources/chapter{chapter}/questions.txt'
    
    print(f"{'='*80}")
    print(f"CHAPTER {chapter}")
    print(f"{'='*80}")
    
    ocp_questions = extract_chapter_questions_detailed(ocp_file, chapter)
    
    with open(questions_file, 'r', encoding='utf-8') as f:
        our_content = f.read()
    
    our_questions = {}
    lines = our_content.split('\n')
    current_q = None
    current_lines = []
    
    for line in lines:
        q_match = re.match(r'^(\d+)\.\s+', line)
        if q_match:
            if current_q is not None:
                our_questions[current_q] = '\n'.join(current_lines)
            current_q = int(q_match.group(1))
            current_lines = [line]
        elif current_q is not None:
            current_lines.append(line)
    
    if current_q is not None:
        our_questions[current_q] = '\n'.join(current_lines)
    
    chapter_issues = []
    
    for q_num in sorted(ocp_questions.keys()):
        ocp_text = ocp_questions[q_num]
        our_text = our_questions.get(q_num, '')
        
        ocp_answers = count_answers_in_text(ocp_text)
        our_answers = count_answers_in_text(our_text)
        
        if len(ocp_answers) != len(our_answers):
            issue = {
                'chapter': chapter,
                'question': q_num,
                'ocp_answers': ocp_answers,
                'our_answers': our_answers,
                'ocp_text': ocp_text,
                'our_text': our_text
            }
            chapter_issues.append(issue)
            print(f"Q{q_num}: OCP has answers {ocp_answers}, we have {our_answers}")
    
    if not chapter_issues:
        print("✓ No issues found")
    
    all_issues.extend(chapter_issues)
    print()

print(f"\n{'='*80}")
print(f"SUMMARY: Found {len(all_issues)} questions with mismatched answers")
print(f"{'='*80}\n")

# Show details for key issues
if all_issues:
    for issue in all_issues[:5]:  # Show first 5
        print(f"\n{'='*80}")
        print(f"Chapter {issue['chapter']}, Question {issue['question']}")
        print(f"OCP answers: {issue['ocp_answers']}")
        print(f"Our answers: {issue['our_answers']}")
        print(f"{'='*80}")
        print("\nOCP version (truncated):")
        print(issue['ocp_text'][:800])
        print("\n---")
        print("Our version (truncated):")
        print(issue['our_text'][:800])

