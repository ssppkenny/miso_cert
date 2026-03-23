#!/usr/bin/env python3
import re
import os

def fix_questions_file(filepath):
    """Fix questions that have text split across multiple lines"""
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    fixed_lines = []
    i = 0
    changes_made = 0
    
    while i < len(lines):
        line = lines[i]
        
        # Check if this is a question line (starts with number and period)
        if re.match(r'^\d+\.\s+', line):
            question_lines = [line.rstrip('\n')]
            i += 1
            
            # Keep reading until we hit an answer line (A., B., C., etc.) or another question
            while i < len(lines):
                next_line = lines[i].strip()
                
                # Stop if we hit an answer choice or another question or empty line followed by answer
                if (re.match(r'^[A-Z]\.\s+', next_line) or 
                    re.match(r'^\d+\.\s+', next_line) or
                    next_line == ''):
                    break
                
                # This line is part of the question text
                question_lines.append(lines[i].rstrip('\n'))
                i += 1
            
            # Join multi-line question into single line if there are continuation lines
            if len(question_lines) > 1:
                # Find lines that are actual continuations (not code blocks)
                text_lines = []
                for qline in question_lines:
                    stripped = qline.strip()
                    # Skip lines that look like code (heavy indentation)
                    if stripped and not qline.startswith('     ') or qline == question_lines[0]:
                        text_lines.append(stripped)
                
                if len(text_lines) > 1:
                    # Join the text lines with a space
                    combined = ' '.join(text_lines)
                    fixed_lines.append(combined + '\n')
                    changes_made += 1
                    print(f"  Fixed question {question_lines[0][:50]}...")
                else:
                    # No actual text continuation, keep as is
                    fixed_lines.extend([line + '\n' for line in question_lines])
            else:
                fixed_lines.append(question_lines[0] + '\n')
        else:
            fixed_lines.append(line)
            i += 1
    
    if changes_made > 0:
        with open(filepath, 'w', encoding='utf-8') as f:
            f.writelines(fixed_lines)
        print(f"Fixed {changes_made} questions in {filepath}")
    else:
        print(f"No changes needed in {filepath}")
    
    return changes_made

def main():
    base_path = '/home/sergey/code/haskell/cert/resources'
    total_changes = 0
    
    for chapter_num in range(6, 15):
        chapter = f'chapter{chapter_num}'
        filepath = os.path.join(base_path, chapter, 'questions.txt')
        
        if os.path.exists(filepath):
            print(f"\n=== Processing {chapter} ===")
            changes = fix_questions_file(filepath)
            total_changes += changes
    
    print(f"\n=== Total: Fixed {total_changes} questions ===")

if __name__ == '__main__':
    main()

