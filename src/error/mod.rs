use core::fmt::{Debug, Display, Error as fmtError, Formatter};
use std::error::Error as stdError;

#[derive(Debug)]
pub struct Error {
	variant: ErrorVariant,
}

#[derive(Debug)]
pub enum ErrorVariant {
	NoDefault,
	Other(Box<dyn stdError>),
}

impl stdError for Error {
	fn source(&self) -> Option<&(dyn stdError + 'static)> {
		match self.variant {
			ErrorVariant::NoDefault => None,
			ErrorVariant::Other(ref source) => Some(source.as_ref()),
		}
	}
}
impl Display for Error {
	fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), fmtError> {
		match self.variant {
			ErrorVariant::NoDefault => writeln!(fmt, "Not provided.")?,
			ErrorVariant::Other(ref o) => write!(fmt, "{:#}", o)?,
		}
		Ok(())
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
			AutoProvisionError::MissingDependency(dependency, source) => match source.variant {
				ErrorVariant::NoDefault => writeln!(fmt, "{} (No default provision.)", dependency),
				ErrorVariant::Other(_) => write!(
					fmt,
					"{}\n\
						-> {:#}",
					dependency, source
				),
			},
		}
	}
}
