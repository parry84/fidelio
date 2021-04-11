CREATE TYPE payload_types AS ENUM ('message', 'image', 'audio', 'video', 'file');

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
    max_views INT NOT NULL,
    payload_type payload_types DEFAULT 'message' NOT NULL
);
