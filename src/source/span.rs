use std::ops::Range;

use codespan_reporting::diagnostic::Label;

#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub(crate) struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end: end.max(start),
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn as_range(&self) -> Range<usize> {
        self.start..self.end
    }

    pub fn union(&self, other: Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn intersection(&self, other: Self) -> Option<Self> {
        let start = self.start.max(other.start);
        let end = self.end.min(other.end);

        if start < end {
            Some(Self { start, end })
        } else {
            None
        }
    }

    pub fn with_file(self, file_id: usize) -> FileSpan {
        FileSpan {
            span: self,
            file_id,
        }
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.as_range()
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_range().fmt(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct FileSpan {
    pub span: Span,
    pub file_id: usize,
}

impl FileSpan {
    pub fn label_primary(self) -> Label<usize> {
        Label::primary(self.file_id, self.span)
    }

    pub fn label_secondary(self) -> Label<usize> {
        Label::secondary(self.file_id, self.span)
    }
}
