-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE secrets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL UNIQUE,
    payload TEXT NOT NULL,
    "password" TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    expires_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    sender TEXT DEFAULT NULL,
    failed_attempts_count INT DEFAULT 0 NOT NULL,
    notify_sender BOOLEAN DEFAULT false NOT NULL,
    view_count INT DEFAULT 0 NOT NULL,
    max_views INT NOT NULL
);
