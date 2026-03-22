#!/usr/bin/env python3
"""
Extract questions and answers for chapters 6-14 from book.txt (v2 - improved)
and write them to resources/chapterN/questions.txt and answers.txt
"""

import re
import os

BOOK_PATH = '/home/sergey/code/haskell/cert/book.txt'
RESOURCES_PATH = '/home/sergey/code/haskell/cert/resources'

# Chapter boundaries in book.txt (1-indexed line numbers)
CHAPTERS = [
    (6,  12053, 15139, 27436, 28484, "Working with Streams and Lambda Expressions"),
    (7,  15140, 16728, 28485, 28951, "Packaging and Deploying Java Code and Use the Java Platform Module System"),
    (8,  16729, 17883, 28952, 29281, "Managing Concurrent Code Execution"),
    (9,  17884, 18915, 29282, 29595, "Using Java I/O API"),
    (10, 18916, 19650, 29596, 29755, "Accessing Databases Using JDBC"),
    (11, 19651, 20261, 29756, 29936, "Implementing Localization"),
    (12, 20262, 21614, 29937, 30306, "Practice Exam 1"),
    (13, 21615, 22938, 30307, 30667, "Practice Exam 2"),
    (14, 22939, 24286, 30668, None,  "Practice Exam 3"),
]

CHAPTER_HEADER_RE = re.compile(
    r'Chapter\s+\d+\s+[\u25a0\u25aa\u2022\ufffd\u25ba■]',
    re.UNICODE
)
APPENDIX_HEADER_RE = re.compile(r'Appendix\s+.*Answers', re.IGNORECASE)
PAGE_NUMBER_RE = re.compile(r'^\s*\d{2,3}\s*$')


def read_book():
    with open(BOOK_PATH, 'r', encoding='utf-8', errors='replace') as f:
        lines = f.readlines()
    return lines


def should_skip_line(line):
    stripped = line.strip()
    if not stripped:
        return False
    if '\f' in line:
        return True
    if CHAPTER_HEADER_RE.search(stripped):
        return True
    if APPENDIX_HEADER_RE.search(stripped):
        return True
    if PAGE_NUMBER_RE.match(stripped):
        return True
    # Chapter reference lines like "Chapter6: Working..." (at start of line, short)
    if re.match(r'^Chapter\s*\d+\s*[:\u00a0]', stripped) and len(stripped) < 100:
        return True
    return False


def clean_lines(raw_lines):
    """Remove page headers and normalize lines."""
    cleaned = []
    for line in raw_lines:
        line = line.replace('\f', '')
        line = line.replace('\u00a0', ' ')
        line = line.replace('\u25aa', '■').replace('\u25a0', '■').replace('\u25ba', '■')
        if should_skip_line(line):
            continue
        cleaned.append(line.rstrip('\n'))
    
    result = []
    prev_blank = False
    for line in cleaned:
        is_blank = not line.strip()
        if is_blank and prev_blank:
            continue
        result.append(line)
        prev_blank = is_blank
    return result


def get_section(all_lines, start_line, end_line):
    start = start_line - 1
    end = end_line if end_line is not None else len(all_lines)
    return all_lines[start:end]


def fix_split_option(lines):
    """
    Fix lines where an option letter is alone on a line with content on the next line.
    E.g.:  "F. "  then  "Source, intermediate operation, and terminal operation"
    """
    result = []
    i = 0
    while i < len(lines):
        line = lines[i]
        m = re.match(r'^([A-Z])\.\s*$', line)
        if m:
            letter = m.group(1)
            j = i + 1
            while j < len(lines) and not lines[j].strip():
                j += 1
            if j < len(lines):
                content_line = lines[j]
                if not re.match(r'^[A-Z]\.\s', content_line) and not re.match(r'^\d+\.\s', content_line):
                    result.append(f"{letter}.  {content_line.strip()}")
                    i = j + 1
                    continue
        result.append(line)
        i += 1
    return result


def extract_questions(section_lines):
    """Extract questions from a section. Returns list of (q_num, lines)."""
    cleaned = clean_lines(section_lines)
    cleaned = fix_split_option(cleaned)
    
    questions = []
    current_q_num = None
    current_q_lines = []
    
    i = 0
    while i < len(cleaned):
        line = cleaned[i]
        m = re.match(r'^(\d+)\.\s{1,4}(.*)$', line)
        if m:
            q_num = int(m.group(1))
            is_sequential = (current_q_num is None or q_num == current_q_num + 1)
            is_restart = (q_num == 1 and current_q_num is not None and current_q_num > 5)
            
            if is_restart:
                # Hit next chapter's questions, stop
                break
            
            if is_sequential:
                if current_q_lines:
                    questions.append((current_q_num, current_q_lines))
                current_q_num = q_num
                current_q_lines = [line]
                i += 1
                continue
        
        if current_q_num is not None:
            current_q_lines.append(line)
        i += 1
    
    if current_q_lines:
        questions.append((current_q_num, current_q_lines))
    
    return questions


def build_questions_output(questions):
    """Build the questions.txt content."""
    output_lines = []
    for q_num, q_lines in questions:
        # Trim leading/trailing blanks from this question
        while q_lines and not q_lines[0].strip():
            q_lines.pop(0)
        while q_lines and not q_lines[-1].strip():
            q_lines.pop()
        
        prev_blank = False
        for line in q_lines:
            is_blank = not line.strip()
            if is_blank and prev_blank:
                continue
            output_lines.append(line)
            prev_blank = is_blank
    
    while output_lines and not output_lines[-1].strip():
        output_lines.pop()
    
    return '\n'.join(output_lines) + '\n'


def extract_answers(section_lines, chapter_num):
    """Extract answers from the answer section. Returns list of (a_num, lines)."""
    cleaned = clean_lines(section_lines)
    
    # Skip chapter header at top
    start_idx = 0
    for i, line in enumerate(cleaned):
        if re.match(r'^\d+\.\s+[A-Z,]', line):
            start_idx = i
            break
    cleaned = cleaned[start_idx:]
    
    answers = []
    current_a_num = None
    current_a_lines = []
    
    for line in cleaned:
        m = re.match(r'^(\d+)\.\s+', line)
        if m:
            a_num = int(m.group(1))
            if current_a_num is None or a_num == current_a_num + 1:
                if current_a_lines:
                    answers.append((current_a_num, current_a_lines))
                current_a_num = a_num
                current_a_lines = [line]
                continue
        
        if current_a_num is not None and line.strip():
            current_a_lines.append(line)
    
    if current_a_lines:
        answers.append((current_a_num, current_a_lines))
    
    return answers


def clean_answer_text(text):
    """Remove embedded chapter references and fix hyphenated word breaks."""
    # Remove trailing book index section (starts with "Index Symbols")
    idx_pos = text.find('Index Symbols')
    if idx_pos > 0:
        text = text[:idx_pos]
    # Remove trailing "Online Test Bank" section
    otb_pos = text.find('Online Test Bank')
    if otb_pos > 0:
        text = text[:otb_pos]
    # Remove "Chapter N: Title" references at end of text
    text = re.sub(r'\s+Chapter\s*\d+\s*[:\-][^.]*?(?=\s*\d+\.|$)', '', text)
    # Remove "Appendix ■ Answers..." embedded in text
    text = re.sub(r'\s+Appendix\s+[■\u25a0]\s+Answers to Review Questions\s*', ' ', text)
    # Fix hyphenated word breaks from PDF: "mak-\ning" -> "making"
    text = re.sub(r'(\w+)-\s+(\w+)', r'\1\2', text)
    # Fix extra whitespace
    text = re.sub(r'\s+', ' ', text).strip()
    return text


def format_answer(a_num, a_lines):
    """Format an answer into a single line."""
    parts = [line.strip() for line in a_lines if line.strip()]
    full_text = ' '.join(parts)
    full_text = clean_answer_text(full_text)
    return full_text


def build_answers_output(answers):
    """Build the answers.txt content (one answer per line)."""
    return '\n'.join(format_answer(n, lines) for n, lines in answers) + '\n'


def update_chapters_txt(chapters_info):
    """Update the chapters.txt file with the new chapter names."""
    chapters_file = os.path.join(RESOURCES_PATH, 'chapters.txt')
    with open(chapters_file, 'r') as f:
        existing = f.read().strip()
    
    existing_lines = [l for l in existing.split('\n') if l.strip()]
    existing_nums = set()
    for line in existing_lines:
        m = re.match(r'^(\d+):', line)
        if m:
            existing_nums.add(int(m.group(1)))
    
    new_lines = list(existing_lines)
    for chapter_num, title in chapters_info:
        if chapter_num not in existing_nums:
            new_lines.append(f"{chapter_num}:{title}")
    
    with open(chapters_file, 'w') as f:
        f.write('\n'.join(new_lines) + '\n')
    
    print(f"Updated chapters.txt with {len(chapters_info)} new chapters")


def main():
    print("Reading book.txt...")
    all_lines = read_book()
    print(f"Total lines: {len(all_lines)}")
    
    chapters_info = []
    
    for chapter_num, q_start, q_end, a_start, a_end, title in CHAPTERS:
        print(f"\n=== Chapter {chapter_num}: {title} ===")
        
        chapter_dir = os.path.join(RESOURCES_PATH, f'chapter{chapter_num}')
        os.makedirs(chapter_dir, exist_ok=True)
        
        # Extract questions
        q_section = get_section(all_lines, q_start, q_end)
        questions = extract_questions(q_section)
        print(f"  {len(questions)} questions extracted")
        
        if questions:
            q_file = os.path.join(chapter_dir, 'questions.txt')
            with open(q_file, 'w', encoding='utf-8') as f:
                f.write(build_questions_output(questions))
            print(f"  Q1: {questions[0][1][0][:70]}")
            print(f"  Q{questions[-1][0]}: {questions[-1][1][0][:70]}")
        
        # Extract answers
        a_section = get_section(all_lines, a_start, a_end)
        answers = extract_answers(a_section, chapter_num)
        print(f"  {len(answers)} answers extracted")
        
        if answers:
            a_file = os.path.join(chapter_dir, 'answers.txt')
            with open(a_file, 'w', encoding='utf-8') as f:
                f.write(build_answers_output(answers))
            print(f"  A1: {format_answer(*answers[0])[:70]}")
            print(f"  A{answers[-1][0]}: {format_answer(*answers[-1])[:70]}")
        
        if questions and answers and len(questions) != len(answers):
            print(f"  ⚠ WARNING: {len(questions)} questions but {len(answers)} answers!")
        
        chapters_info.append((chapter_num, title))
    
    update_chapters_txt(chapters_info)
    print("\n=== Done! ===")


if __name__ == '__main__':
    main()

