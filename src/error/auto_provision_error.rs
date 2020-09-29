use super::Error as rhizomeError;
use core::fmt::{Debug, Display, Error as fmtError, Formatter};
use std::error::Error as stdError;

#[derive(Debug)]
pub enum AutoProvisionError {
	KeyExists,
	MissingDependency(&'static str, rhizomeError),
}
impl stdError for AutoProvisionError {
	fn source(&self) -> Option<&(dyn stdError + 'static)> {
		use AutoProvisionError::*;
		match self {
			KeyExists => None,
			MissingDependency(_, source) => Some(source),
		}
	}
}

impl Display for AutoProvisionError {
	fn fmt(&self, fmt: &mut Formatter) -> Result<(), fmtError> {
		use AutoProvisionError::*;
		match self {
			KeyExists => writeln!(fmt, "Key exists."),
			MissingDependency(dependency, source) => {
				use rhizomeError::*;
				match source {
					NoDefault => writeln!(fmt, "{} (No default provision.)", dependency),
					NoTagMatched => writeln!(
						fmt,
						"{} (Could not find matching tag to provide at.)",
						dependency
					),
					Other(_) => write!(
						fmt,
						"{}
-> {:#}",
						dependency, source
					),
				}
			}
		}
	}
}
