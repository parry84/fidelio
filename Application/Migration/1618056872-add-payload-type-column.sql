CREATE TYPE payload_types AS ENUM ('message', 'image', 'audio', 'video', 'file');

ALTER TABLE secrets
ADD COLUMN payload_type payload_types DEFAULT 'message' NOT NULL;
