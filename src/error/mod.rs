use core::fmt::{Debug, Display, Error as fmtError, Formatter};
use std::error::Error as stdError;

pub mod auto_provision_error;

#[derive(Debug)]
pub enum Error {
	NoDefault,
	NoTagMatched,
	Other(Box<dyn stdError>),
}

impl stdError for Error {
	fn source(&self) -> Option<&(dyn stdError + 'static)> {
		use Error::*;
		match self {
			NoDefault => None,
			NoTagMatched => None,
			Other(source) => Some(source.as_ref()),
		}
	}
}
impl Display for Error {
	fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), fmtError> {
		use Error::*;
		match self {
			NoDefault => writeln!(fmt, "Not provided.")?,
			NoTagMatched => writeln!(fmt, "No matching tag found to default-provide at.")?,
			Other(o) => write!(fmt, "{:#}", o)?,
		}
		Ok(())
	}
}

impl PartialEq for Error {
	fn eq(&self, rhs: &Error) -> bool {
		use Error::*;
		match (self, rhs) {
			(NoDefault, NoDefault) => true,
			(NoTagMatched, NoTagMatched) => true,
			_ => false,
		}
	}
}
