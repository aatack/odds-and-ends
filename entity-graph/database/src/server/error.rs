//! Mapping the crate [`Error`] to an HTTP status and a JSON error body, plus
//! a JSON extractor that reports a bad body as a JSON 400 (not plain text).

use axum::extract::rejection::JsonRejection;
use axum::extract::{FromRequest, Request};
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use axum::Json;
use serde::de::DeserializeOwned;
use serde::Serialize;

use crate::error::Error;

/// The JSON error body: `{"error": <kind>, "message": <text>}`.
#[derive(Serialize)]
struct ErrBody {
    error: &'static str,
    message: String,
}

/// An HTTP error: a status, a short kind and a message.
pub(crate) struct ApiError(pub StatusCode, pub &'static str, pub String);

impl ApiError {
    /// A 400 with the `invalid` kind. Used by later endpoint rounds.
    #[allow(dead_code)]
    pub(crate) fn invalid(message: impl Into<String>) -> Self {
        ApiError(StatusCode::BAD_REQUEST, "invalid", message.into())
    }
}

impl IntoResponse for ApiError {
    fn into_response(self) -> Response {
        let body = ErrBody {
            error: self.1,
            message: self.2,
        };
        (self.0, Json(body)).into_response()
    }
}

impl From<Error> for ApiError {
    fn from(e: Error) -> Self {
        let (status, kind) = match &e {
            Error::Invalid(_) => (StatusCode::BAD_REQUEST, "invalid"),
            Error::Rejected(_) => (StatusCode::BAD_REQUEST, "rejected"),
            Error::UnknownBranch(_) => (StatusCode::NOT_FOUND, "unknown_branch"),
            Error::BranchArchived(_) => (StatusCode::CONFLICT, "branch_archived"),
            Error::UndoRefused(_) => (StatusCode::CONFLICT, "undo_refused"),
            Error::SchemaTooNew { .. } => (StatusCode::INTERNAL_SERVER_ERROR, "schema"),
            Error::Corrupt(_) => (StatusCode::INTERNAL_SERVER_ERROR, "corrupt"),
            Error::Sqlite(_) => (StatusCode::INTERNAL_SERVER_ERROR, "internal"),
            Error::NotImplemented(_) => (StatusCode::INTERNAL_SERVER_ERROR, "not_implemented"),
        };
        ApiError(status, kind, e.to_string())
    }
}

/// Like [`axum::Json`] for request bodies, but a parse or content-type error
/// comes back as a JSON 400 with the `invalid` kind.
pub(crate) struct ApiJson<T>(pub T);

impl<T, S> FromRequest<S> for ApiJson<T>
where
    T: DeserializeOwned,
    S: Send + Sync,
{
    type Rejection = ApiError;

    async fn from_request(req: Request, state: &S) -> Result<Self, Self::Rejection> {
        match Json::<T>::from_request(req, state).await {
            Ok(Json(value)) => Ok(ApiJson(value)),
            Err(rejection) => Err(bad_json(rejection)),
        }
    }
}

fn bad_json(rejection: JsonRejection) -> ApiError {
    ApiError(StatusCode::BAD_REQUEST, "invalid", rejection.body_text())
}
