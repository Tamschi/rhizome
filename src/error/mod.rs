use core::fmt::{Debug, Display, Error as fmtError, Formatter};
use std::error::Error as stdError;

#[derive(Debug)]
pub enum Error {
	NoDefault,
	NoTagMatched,
	Other(Box<dyn stdError>),
}

impl stdError for Error {
	fn source(&self) -> Option<&(dyn stdError + 'static)> {
		match self {
			Error::NoDefault | Error::NoTagMatched => None,
			Error::Other(source) => Some(source.as_ref()),
		}
	}
}
impl Display for Error {
	fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), fmtError> {
		match self {
			Error::NoDefault => writeln!(fmt, "Not provided.")?,
			Error::NoTagMatched => writeln!(fmt, "No matching tag found to default-provide at.")?,
			Error::Other(o) => write!(fmt, "{:#}", o)?,
		}
		Ok(())
	}
}

impl PartialEq for Error {
	fn eq(&self, rhs: &Error) -> bool {
		match (self, rhs) {
			(Error::NoDefault, Error::NoDefault) | (Error::NoTagMatched, Error::NoTagMatched) => {
				true
			}
			_ => false,
		}
	}
}

#[allow(clippy::module_name_repetitions)]
#[derive(Debug)]
pub enum AutoProvisionError {
	KeyExists,
	MissingDependency(&'static str, Error),
}
impl stdError for AutoProvisionError {
	fn source(&self) -> Option<&(dyn stdError + 'static)> {
		match self {
			AutoProvisionError::KeyExists => None,
			AutoProvisionError::MissingDependency(_, source) => Some(source),
		}
	}
}

impl Display for AutoProvisionError {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmtError> {
		match self {
			AutoProvisionError::KeyExists => writeln!(fmt, "Key exists."),
			AutoProvisionError::MissingDependency(dependency, source) => match source {
				Error::NoDefault => writeln!(fmt, "{} (No default provision.)", dependency),
				Error::NoTagMatched => writeln!(
					fmt,
					"{} (Could not find matching tag to provide at.)",
					dependency
				),
				Error::Other(_) => write!(
					fmt,
					"{}\n\
						-> {:#}",
					dependency, source
				),
			},
		}
	}
}
