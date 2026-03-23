#!/usr/bin/env python3
"""
Re-extract questions for chapters 6-14 from ocp_tests.txt.
ocp_tests.txt preserves blanks as multi-space gaps in code; we convert these to underscores.
"""

import re
import os
import shutil

OCP_PATH = '/home/sergey/code/haskell/cert/ocp_tests.txt'
RESOURCES_PATH = '/home/sergey/code/haskell/cert/resources'
PUBLIC_RESOURCES = '/home/sergey/code/haskell/cert/public/resources'

# (chapter_num, q_start_line, q_end_line) -- 1-indexed, inclusive
# These are the line ranges in ocp_tests.txt where questions for each chapter live.
CHAPTER_RANGES = {
    6:  (9583,  11975),
    7:  (11975, 13143),
    8:  (13143, 14145),
    9:  (14145, 14970),
    10: (14970, 15545),
    11: (15545, 16054),
    12: (16054, 17184),
    13: (17184, 18261),
    14: (18261, 26746),
}

# Page header patterns to skip
PAGE_HEADER_RE = re.compile(
    r'^\s*\d{2,3}\s+Chapter\s+\d+|'      # "296  Chapter 8 2 ..."
    r'^Chapter\s+\d+\s+.*\s+\d{2,3}\s*$|'  # "Chapter 8 2 ...  297"
    r'^\s*Chapter\s+\d+\s+2\s+|'          # "Chapter 6 2 Working..."
    r'^\s*\d{2,3}\s+Chapter\s+\d+\s+2\s+', # "212  Chapter 6 2 Working..."
    re.IGNORECASE
)

# Lines that are chapter intro / "THE OCP EXAM TOPICS" blocks - skip these
SKIP_BLOCK_MARKERS = [
    'THE OCP EXAM TOPICS COVERED IN THIS',
    'PRACTICE TEST  INCLUDE THE  FOLLOWING:',
    'PRACTICE TEST INCLUDE THE FOLLOWING:',
]

# Java keywords that indicate a line is code/indented code (not English)
JAVA_KEYWORDS = (
    'public ', 'private ', 'protected ', 'class ', 'interface ', 'import ',
    'package ', 'var ', 'int ', 'long ', 'double ', 'float ', 'boolean ',
    'String ', 'void ', 'return ', 'if ', 'if(', 'for ', 'for(',
    'while ', 'while(', 'do {', 'try {', 'try(', 'catch ', 'catch(',
    'finally', 'throw ', 'throws ', 'new ', 'static ', 'final ',
    'abstract ', 'enum ', 'record ', 'sealed ', 'System.', 'super(',
    'super.', 'this(', 'this.', '//', '/*', '*/', '@', '=>',
    'ServiceLoader', 'Callable', 'Runnable', 'Future', 'Stream.',
    'Collections.', 'Arrays.', 'Collectors.', 'Executors.',
    'LocalDate', 'LocalTime', 'ZonedDateTime', 'DateTimeFormatter',
    'List.', 'Map.', 'Set.', 'Optional.', 'IntStream', 'LongStream',
    'DoubleStream', 'Comparator.', 'Objects.', 'Path.', 'Files.',
    'Connection', 'PreparedStatement', 'ResultSet', 'DriverManager',
    'module ', 'requires ', 'exports ', 'opens ', 'uses ', 'provides ',
)


def looks_like_code(line):
    stripped = line.strip()
    if not stripped:
        return False
    if stripped.endswith(('{', ';', '}')):
        return True
    for kw in JAVA_KEYWORDS:
        if stripped.startswith(kw):
            return True
    # Has code characters and looks like a statement
    if re.search(r'[;{}()\[\]<>]', stripped) and not re.search(r'\b(choose|which|what|how|given|when|where|suppose|assume|consider)\b', stripped, re.I):
        return True
    return False


def is_page_header(line):
    return bool(PAGE_HEADER_RE.match(line))


def is_chapter_intro_line(line):
    stripped = line.strip()
    for marker in SKIP_BLOCK_MARKERS:
        if marker in stripped.upper():
            return True
    # Lines like "2   Use Java object..." (bullet points in intro)
    if re.match(r'^\s*2\s{3,}', line) and len(stripped) < 120:
        return True
    return False


def is_question_start(line):
    """Detect question line: '1.  text' or '  1.   text' etc."""
    return bool(re.match(r'^\s{0,8}\d+\.\s{1,6}\S', line))


def is_option_start(line):
    """Detect option line: 'A.  text' or '     A.   text'"""
    return bool(re.match(r'^\s{0,8}[A-H]\.\s{1,6}\S', line))


def get_question_num(line):
    m = re.match(r'^\s{0,8}(\d+)\.\s', line)
    if m:
        return int(m.group(1))
    return None


def convert_blanks_in_code(line):
    """
    Convert multi-space sequences representing blanks in Java code to underscores.
    
    Two cases:
    1. In-line gaps: 4+ consecutive spaces between/after non-space characters
       Example: "stream.        );" -> "stream.____________);"
    2. The function is called AFTER leading whitespace normalization, so leading
       blank detection is handled separately.
    """
    # Case 1: spaces between or after non-space content (but not before it)
    # Replace 4+ consecutive spaces that come after at least one non-space character
    def replace_gap(m):
        n = len(m.group(0))
        return '_' * max(n, 10)  # at least 10 underscores for readability
    
    # Replace gaps in middle and at end of line (after content)
    result = re.sub(r'(?<=\S) {4,}', replace_gap, line)
    return result


def normalize_leading_blank(line, min_indent_in_block):
    """
    If a code line has leading spaces > min_indent_in_block + 4,
    the excess represents a blank type/expression.
    Convert to: '_' * excess + stripped content with min_indent indentation.
    """
    stripped = line.lstrip()
    leading = len(line) - len(stripped)
    
    if leading > min_indent_in_block + 4 and stripped:
        excess = leading - min_indent_in_block
        blanks = '_' * max(excess, 10)
        # Reconstruct with min_indent spaces + blanks + space + content
        return ' ' * min_indent_in_block + blanks + ' ' + stripped
    return line


def process_code_block(code_lines):
    """
    Process a list of code lines:
    - Detect minimum indentation
    - Apply leading blank normalization for anomalously indented lines
    - Apply in-line gap -> underscore conversion
    """
    if not code_lines:
        return code_lines
    
    # Compute indentation for each non-empty line
    indents = []
    for line in code_lines:
        stripped = line.strip()
        if stripped:
            indents.append(len(line) - len(line.lstrip()))
    
    if not indents:
        return code_lines
    
    min_indent = min(indents)
    
    result = []
    for line in code_lines:
        stripped = line.strip()
        if not stripped:
            result.append(line)
            continue
        
        leading = len(line) - len(line.lstrip())
        
        # Check if this line has anomalous leading whitespace (leading blank pattern)
        if leading > min_indent + 4:
            line = normalize_leading_blank(line, min_indent)
        
        # Apply in-line gap conversion
        line = convert_blanks_in_code(line)
        
        result.append(line)
    
    return result


def clean_line(line):
    """Remove page headers, normalize characters."""
    # Remove form feeds
    line = line.replace('\f', '')
    # Normalize NBSP
    line = line.replace('\u00a0', ' ')
    return line


def strip_question_prefix_whitespace(lines):
    """
    Questions and options in ocp_tests.txt are sometimes indented.
    Strip consistent leading whitespace from question/option lines.
    """
    # Find the indentation of question numbers to remove it
    result = []
    for line in lines:
        # Strip leading spaces from question/option start lines
        stripped = re.sub(r'^\s{1,8}(\d+\.\s)', r'\1', line)
        if stripped != line:
            line = stripped
        stripped2 = re.sub(r'^\s{1,8}([A-H]\.\s)', r'\1', line)
        if stripped2 != line:
            line = stripped2
        result.append(line)
    return result


def extract_chapter_questions(all_lines, q_start, q_end):
    """
    Extract questions from lines[q_start-1 : q_end-1].
    Returns list of (q_num, raw_lines_list) where raw_lines_list are the lines of the question.
    """
    section = all_lines[q_start - 1 : q_end - 1]
    
    questions = []
    current_q_num = None
    current_q_lines = []
    in_intro_block = False
    
    for line in section:
        line = clean_line(line).rstrip('\n')
        
        # Skip page headers
        if is_page_header(line):
            continue
        
        # Skip intro blocks (chapter objective descriptions)
        stripped = line.strip()
        if is_chapter_intro_line(line):
            in_intro_block = True
            continue
        
        if in_intro_block:
            # End intro block when we hit the first question
            if is_question_start(line):
                in_intro_block = False
            else:
                continue
        
        # Check for question start
        if is_question_start(line):
            q_num = get_question_num(line)
            
            # Check for sequential numbering
            is_sequential = (current_q_num is None or q_num == current_q_num + 1)
            # Also handle case where q_num could restart at 1 for the next chapter
            is_restart = (q_num == 1 and current_q_num is not None and current_q_num > 5)
            
            if is_restart:
                break
            
            if is_sequential or (q_num > 0 and current_q_num is not None and q_num > current_q_num):
                if current_q_lines:
                    questions.append((current_q_num, current_q_lines))
                current_q_num = q_num
                current_q_lines = [line]
                continue
        
        if current_q_num is not None:
            current_q_lines.append(line)
    
    if current_q_lines:
        questions.append((current_q_num, current_q_lines))
    
    return questions


def format_question(q_num, raw_lines):
    """
    Format a question's lines into the final output format.
    - Strip consistent leading indentation
    - Detect and process code blocks
    - Convert space-blanks to underscores
    - Join continuation lines
    """
    # First pass: strip leading whitespace from question/option lines
    lines = strip_question_prefix_whitespace(raw_lines)
    
    # Second pass: identify code blocks and process them
    # Group lines into: question text, option text, code lines
    result_lines = []
    code_buffer = []
    
    def flush_code():
        if code_buffer:
            processed = process_code_block(code_buffer)
            result_lines.extend(processed)
            code_buffer.clear()
    
    for line in lines:
        stripped = line.strip()
        
        if not stripped:
            flush_code()
            # Skip blank lines (normalize_questions.py will handle spacing)
            continue
        
        if is_option_start(line) or is_question_start(line):
            flush_code()
            # Apply in-line blank conversion to question/option text too
            line = convert_blanks_in_code(line)
            result_lines.append(line.rstrip())
            continue
        
        if looks_like_code(line):
            code_buffer.append(line.rstrip())
        else:
            flush_code()
            # Apply in-line blank conversion  
            line = convert_blanks_in_code(line)
            result_lines.append(line.rstrip())
    
    flush_code()
    
    # Clean up: remove trailing blank lines
    while result_lines and not result_lines[-1].strip():
        result_lines.pop()
    
    return result_lines


def build_output(questions):
    """Build final questions.txt content from list of (q_num, lines) pairs."""
    output_lines = []
    for q_num, raw_lines in questions:
        formatted = format_question(q_num, raw_lines)
        for line in formatted:
            output_lines.append(line)
        # Add blank line between questions for readability
        output_lines.append('')
    
    while output_lines and not output_lines[-1].strip():
        output_lines.pop()
    
    return '\n'.join(output_lines) + '\n'


def normalize_output(content):
    """
    Apply normalize_questions.py logic:
    - Remove blank lines
    - Join continuation lines onto question/option
    - Keep code lines separate
    """
    CODE_STARTS = (
        'public ', 'private ', 'protected ', 'class ', 'interface ', 'import ',
        'package ', 'var ', 'int ', 'long ', 'double ', 'float ', 'boolean ',
        'String ', 'void ', 'return ', 'if (', 'if(', 'for (', 'for(',
        'while (', 'while(', 'do {', 'try {', 'try(', 'catch (', 'catch(',
        'finally', 'throw ', 'throws ', 'new ', 'static ', 'final ',
        'abstract ', 'enum ', 'record ', 'sealed ', 'System.', 'super(',
        'super.', 'this(', 'this.', '//', '/*', '*/', '@', '=>', '} }',
        'ServiceLoader', 'Callable', 'Runnable', 'Future', 'Stream.',
        'Collections.', 'Arrays.', 'Collectors.', 'Executors.',
        'LocalDate', 'LocalTime', 'ZonedDateTime', 'DateTimeFormatter',
        'List.', 'Map.', 'Set.', 'Optional.', 'IntStream', 'LongStream',
        'DoubleStream', 'module ', 'requires ', 'exports ', 'opens ', 'uses ',
        'provides ', 'Connection', 'PreparedStatement', 'ResultSet',
    )
    
    def _is_q(line):
        return bool(re.match(r'^\d+\.\s{1,4}', line))
    
    def _is_opt(line):
        return bool(re.match(r'^[A-H]\.\s{1,4}', line))
    
    def _is_code(line):
        stripped = line.strip()
        if not stripped:
            return False
        if re.match(r'^\s{3,}', line):
            return True
        if stripped.endswith(('{', ';', '}')):
            return True
        for s in CODE_STARTS:
            if stripped.startswith(s):
                return True
        if re.search(r'\(.*\)', stripped) and re.search(r'[=;{]', stripped):
            return True
        # Lines with underscores in code context
        if re.search(r'_{4,}', stripped) and re.search(r'[.=(;]', stripped):
            return True
        return False
    
    raw_lines = content.split('\n')
    non_blank = [l.rstrip() for l in raw_lines if l.strip()]
    
    if not non_blank:
        return ''
    
    result = []
    for line in non_blank:
        if not result:
            result.append(line)
            continue
        
        if _is_q(line) or _is_opt(line):
            result.append(line)
            continue
        
        if _is_code(line):
            result.append(line)
            continue
        
        # Continuation line: join to previous
        prev = result[-1]
        prev_ends_mid = prev.rstrip() and prev.rstrip()[-1] not in '.?!:)}]'
        if _is_q(prev) or _is_opt(prev) or prev_ends_mid:
            result[-1] = prev + ' ' + line.strip()
        else:
            result.append(line)
    
    return '\n'.join(result) + '\n'


def main():
    print(f"Reading {OCP_PATH}...")
    with open(OCP_PATH, 'r', encoding='utf-8', errors='replace') as f:
        all_lines = f.readlines()
    print(f"Total lines: {len(all_lines)}")
    
    for ch_num in sorted(CHAPTER_RANGES.keys()):
        q_start, q_end = CHAPTER_RANGES[ch_num]
        print(f"\n=== Chapter {ch_num} (lines {q_start}-{q_end}) ===")
        
        questions = extract_chapter_questions(all_lines, q_start, q_end)
        print(f"  Extracted {len(questions)} questions")
        
        if not questions:
            print(f"  WARNING: No questions found!")
            continue
        
        # Show first and last question
        first_num, first_lines = questions[0]
        last_num, last_lines = questions[-1]
        print(f"  Q{first_num}: {first_lines[0][:70]}")
        print(f"  Q{last_num}: {last_lines[0][:70]}")
        
        # Build output
        raw_output = build_output(questions)
        final_output = normalize_output(raw_output)
        
        # Write to resources
        ch_dir = os.path.join(RESOURCES_PATH, f'chapter{ch_num}')
        os.makedirs(ch_dir, exist_ok=True)
        q_file = os.path.join(ch_dir, 'questions.txt')
        
        with open(q_file, 'w', encoding='utf-8') as f:
            f.write(final_output)
        print(f"  Written to {q_file}")
        
        # Sync to public
        pub_dir = os.path.join(PUBLIC_RESOURCES, f'chapter{ch_num}')
        pub_file = os.path.join(pub_dir, 'questions.txt')
        if os.path.exists(pub_dir):
            shutil.copy2(q_file, pub_file)
            print(f"  Synced to {pub_file}")
        elif os.path.isdir(PUBLIC_RESOURCES):
            os.makedirs(pub_dir, exist_ok=True)
            shutil.copy2(q_file, pub_file)
            print(f"  Copied to {pub_file}")
    
    print("\n=== Done! ===")


if __name__ == '__main__':
    main()

