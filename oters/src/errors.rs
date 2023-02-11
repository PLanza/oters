use crate::parser::span::Spanned;
pub type SpError = Spanned<anyhow::Error>;

impl SpError {
    pub fn new(err: anyhow::Error, span: (usize, usize)) -> Self {
        Spanned {
            span,
            term: Box::new(err),
        }
    }

    // Constructs an error message given the source code and the location of the error
    // Location is given as start and end byte positions
    pub fn get_error_string(self, source: &String) -> String {
        let location = self.span;

        let mut line_number = 1;
        let mut line_start = 0;
        let mut line_end = 0;

        // The token's position within its line by number of characters
        let mut token_pos = 0;

        // Convert byte positions to line and character positions
        for (i, c) in source.char_indices() {
            if c == '\n' && i < location.0 {
                line_start = i + 1;
                line_number += 1;
                token_pos = 0;
                continue;
            }

            if c == '\n' && i >= location.0 {
                line_end = i;
                break;
            }

            if i < location.0 {
                token_pos += 1;
            }
        }

        // The source code line where the error occurs
        let code_line = &source[line_start..line_end];

        // A line pointing where the error is located
        // Assumes all utf8 characters are displayed with the same width
        let mut under_line = (0..token_pos).map(|_| " ").collect::<String>();
        under_line.push_str(
            &String::from_utf8(vec![b'^'; source[location.0..location.1].len()]).unwrap(),
        );

        let message = format!(
            "{} on line {}\n{}\n{}",
            self.term, line_number, code_line, under_line
        );

        message.to_string()
    }

    pub fn to_anyhow(self, source: &String) -> anyhow::Error {
        anyhow::anyhow!(self.get_error_string(source))
    }
}
