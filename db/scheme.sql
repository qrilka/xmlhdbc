CREATE SEQUENCE seq_login_id
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE TABLE login (
    id integer DEFAULT nextval('seq_login_id'::regclass) NOT NULL,
    login character varying(62) NOT NULL,
    pwhash text NOT NULL
);

CREATE TABLE userdetails (
    login integer NOT NULL,
    name text NOT NULL
);

CREATE TABLE permission (
    login integer NOT NULL,
    permission character varying(128) NOT NULL,
    CONSTRAINT check_permission CHECK (((permission)::text ~ '^[*0-9a-zA-Z]+$'::text))
);

CREATE TABLE metapermission (
    name character varying(64) NOT NULL,
    permission character varying(64) NOT NULL,
    CONSTRAINT check_name CHECK (((name)::text ~ '\\w+'::text))
);

CREATE SEQUENCE seq_guid
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;

CREATE TABLE websession (
    guid character varying(36) DEFAULT f_guid() NOT NULL,
    ts timestamp without time zone DEFAULT now(),
    login integer NOT NULL
);


ALTER TABLE ONLY login
    ADD CONSTRAINT login_login_key UNIQUE (login);

ALTER TABLE ONLY login
    ADD CONSTRAINT login_pkey PRIMARY KEY (id);

ALTER TABLE ONLY metapermission
    ADD CONSTRAINT metapermission_pkey PRIMARY KEY (name, permission);

ALTER TABLE ONLY websession
    ADD CONSTRAINT websession_pkey PRIMARY KEY (guid);


CREATE VIEW vsessionpermission AS
    SELECT w.guid, l.login, p.permission FROM 
      ((websession w
        JOIN permission p ON ((w.login = p.login)))
        JOIN login l ON ((l.id = w.login)))
  UNION
    SELECT w.guid, l.login, (m.permission)::character varying(128) AS permission FROM
      (((websession w
        JOIN permission p ON ((w.login = p.login)))
        JOIN login l ON ((l.id = w.login)))
        JOIN metapermission m ON (((m.name)::text = (p.permission)::text)));



CREATE FUNCTION digest(text, text) RETURNS bytea
    LANGUAGE c IMMUTABLE STRICT
    AS '$libdir/pgcrypto', 'pg_digest';

CREATE FUNCTION f_guid() RETURNS text
    LANGUAGE sql
    AS $$
 SELECT md5(now() || nextval('seq_guid'::regclass)::text);
$$;

CREATE FUNCTION f_login(p_login text, p_passw text) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
	p_guid text = null;
	p_hash text;
BEGIN
	select pwhash into p_hash from login where login = p_login;
    
	if encode(digest(p_passw, 'sha512'), 'hex') = p_hash
	then
		p_guid = f_guid();
		insert into websession (guid, login)
		select p_guid, id from login where login = p_login;
	end if;

	return p_guid;
END;
$$;

CREATE FUNCTION f_sessiondata_xml(text) RETURNS xml
    LANGUAGE sql IMMUTABLE
    AS $_$
	select xmlelement( name sessiondata
	                  ,xmlattributes($1 as xsid, l.login as login)
					  ,xmlelement(name userdetails, xmlforest(u.name as name))
					  ,(select xmlelement(name permissions, xmlagg(xmlelement(name permission, xmlattributes(p.permission as name)))) from permission p where p.login = l.id)
					  )
	from websession w join login l on w.login = l.id
	                  left join userdetails u on u.login = l.id
	where w.guid = $1;

$_$;
