#!/usr/bin/env python3
"""
Normalize questions.txt files:
- Remove all blank lines
- Join continuation lines onto the previous question/option line
- Keep code blocks as separate lines
"""

import re
import os
import sys

RESOURCES_PATH = '/home/sergey/code/haskell/cert/resources'

# Java keywords and patterns that indicate a code line
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
)


def is_question_start(line):
    """Line starts a new question: '1.  text' or '10.  text'"""
    return bool(re.match(r'^\d+\.\s{1,4}', line))


def is_option_start(line):
    """Line starts an answer option: 'A.  text'"""
    return bool(re.match(r'^[A-Z]\.\s{1,4}', line))


def looks_like_code(line):
    """Heuristic: does this line look like a Java code line?"""
    stripped = line.strip()
    if not stripped:
        return False

    # Has 3+ leading spaces (indented code)
    if re.match(r'^\s{3,}', line):
        return True

    # Ends with code-typical characters
    if stripped.endswith(('{', ';', '}')):
        return True

    # Starts with Java keywords/patterns
    for start in CODE_STARTS:
        if stripped.startswith(start):
            return True

    # Looks like a method call or assignment: contains () and = or ;
    if re.search(r'\(.*\)', stripped) and re.search(r'[=;{]', stripped):
        return True

    return False


def normalize_questions_content(content):
    """
    Normalize questions.txt content:
    1. Remove blank lines
    2. Join continuation lines onto question/option lines
    3. Keep code lines separate
    """
    raw_lines = content.split('\n')

    # Remove blank lines first
    non_blank = [l.rstrip() for l in raw_lines if l.strip()]

    if not non_blank:
        return ''

    result = []

    for line in non_blank:
        if not result:
            result.append(line)
            continue

        # Is this line starting a new element?
        if is_question_start(line) or is_option_start(line):
            result.append(line)
            continue

        # Is this a code line? Keep separate.
        if looks_like_code(line):
            result.append(line)
            continue

        # It's a continuation/plain text line.
        # Join to the previous line if previous was a question or option.
        prev = result[-1]
        prev_is_q_or_opt = is_question_start(prev) or is_option_start(prev)

        # Also join if previous line ends mid-sentence (no terminal punctuation)
        prev_stripped = prev.rstrip()
        prev_ends_mid = prev_stripped and prev_stripped[-1] not in '.?!:)}]'

        if prev_is_q_or_opt or prev_ends_mid:
            # Join with a space
            result[-1] = prev + ' ' + line.strip()
        else:
            # Keep as separate line (e.g., standalone code-like content)
            result.append(line)

    return '\n'.join(result) + '\n'


def normalize_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()

    normalized = normalize_questions_content(content)

    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(normalized)

    # Count lines before and after
    before = len([l for l in content.split('\n') if l.strip()])
    after = len([l for l in normalized.split('\n') if l.strip()])
    return before, after


def main():
    # Process chapters 6-14 (chapters 1-5 are already manually formatted)
    chapters = list(range(6, 15))

    if len(sys.argv) > 1:
        # Allow specifying chapters on command line
        chapters = [int(x) for x in sys.argv[1:]]

    for chapter_num in chapters:
        q_file = os.path.join(RESOURCES_PATH, f'chapter{chapter_num}', 'questions.txt')
        if not os.path.exists(q_file):
            print(f"Chapter {chapter_num}: questions.txt not found, skipping")
            continue

        before, after = normalize_file(q_file)
        print(f"Chapter {chapter_num}: {before} non-blank lines → {after} lines")

    print("\nDone. Syncing to public/resources/...")

    # Also sync to public/resources
    public_resources = '/home/sergey/code/haskell/cert/public/resources'
    for chapter_num in chapters:
        src = os.path.join(RESOURCES_PATH, f'chapter{chapter_num}', 'questions.txt')
        dst_dir = os.path.join(public_resources, f'chapter{chapter_num}')
        dst = os.path.join(dst_dir, 'questions.txt')
        if os.path.exists(src) and os.path.exists(dst_dir):
            import shutil
            shutil.copy2(src, dst)
            print(f"  Synced chapter{chapter_num}/questions.txt to public/")
        elif os.path.exists(src):
            os.makedirs(dst_dir, exist_ok=True)
            import shutil
            shutil.copy2(src, dst)
            print(f"  Copied chapter{chapter_num}/questions.txt to public/ (new dir)")


if __name__ == '__main__':
    main()

