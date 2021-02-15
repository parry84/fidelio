-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE secrets (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL UNIQUE,
    payload TEXT NOT NULL,
    "password" TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL,
    expires_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
